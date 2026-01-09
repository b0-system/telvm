(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Base parameters *)

let guest_archs = Os.Arch.[arm64; x86_64]

let system ~guest_arch = match guest_arch with
| Os.Arch.Arm64 _ -> Ok (Cmd.tool "qemu-system-aarch64")
| Os.Arch.X86_64 _ -> Ok (Cmd.tool "qemu-system-x86_64")
| a -> Fmt.error "%a: Don't know which qemu system to use" Os.Arch.pp_id a

let system_version system = Adhoc.tool_version system

let accel ~host_os ~host_arch ~guest_arch ~accel = match accel with
| Some accel -> accel
| None ->
    let default = "tcg" in
    if not (Os.Arch.equal host_arch guest_arch) then default else
    match host_os with
    | Os.Name.Darwin _ -> "hvf"
    | Os.Name.Linux _ -> "kvm"
    | Os.Name.Windows _ -> "whpx"
    | name ->
        Log.warn begin fun m ->
          m "%a host: Don't know which accelerator to use falling back to %a"
            Os.Name.pp_id name Fmt.code default
        end;
        default

let cpu ~host_arch ~guest_arch =
  if true then  (* Let's see if that works well. *) Ok "max" else
  if Os.Arch.equal host_arch guest_arch then Ok "host" else
  match guest_arch with
  | Os.Arch.Arm64 _ -> Ok "cortex-a53"
  | Os.Arch.X86_64 _ ->
      (* We tried "qemu64" but that would trip the Win installer
         However that doesn't sit well with validation OS it seems. *)
      Ok "Skylake-Client"
  | a -> Fmt.error "%a guest: Don't know which CPU to use" Os.Arch.pp_id a

let machine ~guest_arch = match guest_arch with
| Os.Arch.Arm64 _ -> Ok "virt"
| Os.Arch.X86_64 _ -> Ok "q35"
| a -> Fmt.error "%a guest: Don't know which machine to use" Os.Arch.pp_id a

let mem m = Option.value ~default:"4G" m
let smp v = Option.value ~default:"4" v

let base_cmd ~system ~machine ~accel:a ~cpu ~smp ~mem =
  Cmd.(system % "-machine" % machine % "-accel" % a % "-cpu" % cpu %
       "-smp" % smp % "-m" % mem)

let auto_base_cmd ~host_os ~host_arch ~guest_arch ~accel:a ~smp:s ~mem:m =
  let* system = system ~guest_arch in
  let* machine = machine ~guest_arch in
  let accel = accel ~host_os ~host_arch ~guest_arch ~accel:a in
  let* cpu = cpu ~host_arch ~guest_arch in
  let smp = smp s in
  let mem = mem m in
  Ok (base_cmd ~system ~machine ~accel ~cpu ~smp ~mem)

(* Bootloader parameters *)

(* UEFI code lookup

   This doesn't seem to be well documented it works with at least
   homebrew and debian. We ask the used qemu system for its lookup
   paths with by invoking [system -L help].  Then we look in each of
   these paths for one of the files mentioned in
   [firmware_json_filename] in a [firmware] subdirectory we query the
   json in the file for the firmware executable.

   For now we eschew that the nvram I guess it's useful to persist bootloader
   params but are we interested ? Note that we'd need one per vm run
   let efi_vars_drive =
     Fmt.str "if=pflash,format=raw,file=%s" "edk2-i386-vars.fd" *)

let firmware_json_filename ~host_os:_ ~guest_arch = match guest_arch with
| Os.Arch.Arm32 _ -> Ok "60-edk2-arm.json"
| Os.Arch.Arm64 _ -> Ok "60-edk2-aarch64.json"
| Os.Arch.X86_64 _ -> Ok "60-edk2-x86_64.json"
| Os.Arch.X86_32 _ -> Ok "60-edk2-i386.json"
| Os.Arch.Riscv64 _ -> Ok "60-edk2-riscv64.json"
| arch ->
    Fmt.error "%a guest: Don't know which .json firmware file to use"
      Os.Arch.pp_id arch

let find_firmware_json ~host_os ~guest_arch =
  let* firmware_json_filename = firmware_json_filename ~host_os ~guest_arch in
  let* system = system ~guest_arch in
  let* dirs = Os.Cmd.run_out ~trim:true Cmd.(system % "-L" % "help") in
  let clean dir =
    if dir = "" then None else
    Log.if_error ~use:None @@
    Result.map Option.some (Fpath.of_string (String.trim dir))
  in
  let dirs = List.filter_map clean (String.split_all ~sep:"\n" dirs) in
  let rec loop = function
  | [] ->
      Fmt.error "@[<v>Could not find firmware .json file %s in directories:@,%a"
        firmware_json_filename Fmt.(list Fpath.pp) dirs
  | dir :: dirs ->
      let file =
        Log.if_error ~use:None @@
        Os.Path.exists_realpath
          Fpath.(dir / "firmware" / firmware_json_filename)
      in
      match file with
      | Some file -> Ok file
      | None -> loop dirs
  in
  loop dirs

let firmware_executable_of_json file =
  let open B0_json in
  let executableq =
    Jsonq.(succeed (fun x y -> x, y) $
           mem "filename" string $
           mem "format" string)
  in
  let query = Jsonq.(mem "mapping" @@ mem "executable" @@ executableq) in
  let* json = Os.File.read file in
  let* json = Json.of_string ~file:(Fpath.to_string file) json in
  let* exe, format = Jsonq.query query json in
  let* exe = Fpath.of_string exe in
  Ok (exe, format)

let uefi_code ~host_os ~guest_arch =
  let err e = Fmt.str "@[Could not locate UEFI code:@,%a@]" Fmt.lines e in
  Result.map_error err @@
  let* firmware_json = find_firmware_json ~host_os ~guest_arch in
  let* file, format = firmware_executable_of_json firmware_json in
  let* () = Os.File.must_exist file in
  Ok (file, format)

let uefi_drive ~uefi_code:(code, format) =
  let code = Fpath.to_string code in
  let drive = Fmt.str "if=pflash,format=%s,readonly=on,file=%s" format code in
  Cmd.(arg "-drive" % drive)

(* Networking parameters *)

let nic_virtio_net_pci = "user,model=virtio-net-pci"

(* USB devices *)

let device_usb_qemu_xhci () = Cmd.(arg "-device" % "qemu-xhci")

let usb_input_devices () =
  let devices = ["usb-kbd"; "usb-tablet"] in
  Cmd.of_list ~slip:"-device" Fun.id devices

(* Drives *)

type drive_device =
| Nvme | Sata | Sata_cdrom | Smb | Usb | Usb_cdrom | Virtio_9p_pci | Virtio_blk
| Virtio_scsi | Virtio_scsi_cdrom

let drive_devices =
  [ Nvme; Sata; Sata_cdrom; Smb; Usb; Usb_cdrom; Virtio_9p_pci; Virtio_blk;
    Virtio_scsi; Virtio_scsi_cdrom ]

let drive_device_to_string = function
| Nvme -> "nmve"
| Sata -> "sata"
| Sata_cdrom -> "sata-cdrom"
| Smb -> "smb"
| Usb -> "usb"
| Usb_cdrom -> "usb-cdrom"
| Virtio_9p_pci -> "virtio-9p-pci"
| Virtio_blk -> "virtio-blk"
| Virtio_scsi -> "virtio-scsi"
| Virtio_scsi_cdrom -> "virtio-scsi-cdrom"

let drive_device_of_string = function
| "nvme" -> Ok Nvme
| "sata" -> Ok Sata
| "sata-cdrom" -> Ok Sata_cdrom
| "smb" -> Ok Smb
| "usb" -> Ok Usb
| "usb-cdrom" -> Ok Usb_cdrom
| "virtio-9p-pci" -> Ok Virtio_9p_pci
| "virtio-blk" -> Ok Virtio_blk
| "virtio-scsi" -> Ok Virtio_scsi
| "virtio-scsi-cdrom" -> Ok Virtio_scsi_cdrom
| kind ->
    Fmt.error "%s: unknown drive device. %a"
      kind Fmt.(must_be code) (List.map drive_device_to_string drive_devices)

let drive_device_of_path path =
  match String.split_last ~sep:"@" (Fpath.to_string path) with
  | None -> Ok (None, path)
  | Some ("", p) -> Ok (None, path)
  | Some (p, device) ->
      let is_perhaps_windows_drive = String.length p = 1 in
      match drive_device_of_string device with
      | Ok device -> Ok (Some device, Fpath.v p)
      | Error e when is_perhaps_windows_drive -> Ok (None, path)
      | Error e -> Fmt.error "%a: @[%a@]" Fpath.pp path Fmt.styled_text e

type drive = drive_device * Fpath.t

let device ?(bus = "") ?(serial = false) ~driveid ~devid ~bootidx ~driver () =
  let serial = if not serial then "" else Fmt.str "serial=%s," devid in
  let bus = if bus = "" then "" else Fmt.str "bus=%s," bus in
  Fmt.str "drive=%s,id=%s,%sbootindex=%d,%sdriver=%s"
    driveid devid bus bootidx serial driver

let add_drive_cmd id ?bus ?serial ?(cache = "") ~image ~driver () =
  let driveid = Fmt.str "drive%02d" id in
  let devid = Fmt.str "dev%02d" id in
  let drive =
    let cache = if cache = "" then "" else Fmt.str "cache=%s," cache in
    let format =
      if Fpath.has_ext ".img" image || Fpath.has_ext ".iso" image
      then "format=raw," else ""
    in
    Fmt.str "if=none,id=%s,%s%sfile=%a" driveid cache format Fpath.pp image
  in
  Cmd.(arg "-drive" % drive %
       "-device" % device ?bus ?serial ~driveid ~devid ~bootidx:id ~driver ())

let add_cdrom_drive_cmd id ~image ~driver =
  let driveid = Fmt.str "drive%02d" id in
  let devid = Fmt.str "dev%02d" id in
  let drive =
    Fmt.str
      "if=none,id=%s,media=cdrom,format=raw,file=%a" driveid Fpath.pp image
  in
  Cmd.(arg "-drive" % drive %
       "-device" % device ~driveid ~devid ~bootidx:id ~driver ())

let add_virtio_9p_pci_cmd id ~dir =
  let fsid = Fmt.str "fsdev%02d" id in
  let devid = Fmt.str "dev%02d" id in
  let mount_tag = Fpath.basename dir in
  let fsdev =
    Fmt.str
      "local,security_model=mapped-xattr,id=%s,path=%a" fsid Fpath.pp dir
  in
  let device =
    Fmt.str "virtio-9p-pci,id=%s,fsdev=%s,mount_tag=%s" devid fsid mount_tag
  in
  Cmd.(arg "-fsdev" % fsdev %
       "-device"  % device)

let add_smb_cmd id ~dir =
  let devid = Fmt.str "dev%02d" id in
  let nic =
    Fmt.str "user,id=%s,smb=%a" devid Fpath.pp dir
  in
  Cmd.(arg "-nic" % nic)

let drives_cmd ds =
  let add_drive (id, cmd, add_scsi_controller) d =
    let drive_cmd, add_scsi = match d with
    | Usb, image ->
        add_drive_cmd id ~image ~driver:"usb-storage" (),
        add_scsi_controller
    | Usb_cdrom, image ->
        add_cdrom_drive_cmd id ~image ~driver:"usb-storage",
        add_scsi_controller
    | Virtio_9p_pci, dir ->
        add_virtio_9p_pci_cmd id ~dir,
        add_scsi_controller
    | Virtio_blk, image ->
        add_drive_cmd id ~cache:"none" ~image ~driver:"virtio-blk" (),
        add_scsi_controller
    | Virtio_scsi, image ->
        add_drive_cmd id ~cache:"none" ~image ~driver:"scsi-hd" (),
        true
    | Virtio_scsi_cdrom, image ->
        add_cdrom_drive_cmd id ~image ~driver:"scsi-cd",
        true
    | Sata, image ->
        add_drive_cmd id ~cache:"none" ~serial:true ~image ~driver:"ide-hd" (),
        add_scsi_controller
    | Sata_cdrom, image ->
        let bus = Fmt.str "ide.%d" id in
        add_drive_cmd id ~bus ~image ~driver:"ide-cd" (),
        add_scsi_controller
    | Smb, dir ->
        add_smb_cmd id ~dir,
        add_scsi_controller
    | Nvme, image ->
        add_drive_cmd id ~cache:"none" ~serial:true ~image ~driver:"nvme" (),
        add_scsi_controller
    in
    id + 1, Cmd.(cmd %% drive_cmd), add_scsi
  in
  let init = (1, Cmd.empty, false) in
  let _, cmd, add_scsi_controller = List.fold_left add_drive init ds in
  if not add_scsi_controller
  then cmd
  else Cmd.(arg "-device" % "virtio-scsi-pci" %% cmd)

(* Graphics *)

let device_virtio_gpu_pci = Cmd.(arg "-device" % "virtio-gpu-pci")
let device_ramfb = Cmd.(arg "-device" % "ramfb")
let no_graphic = Cmd.(arg "-nographic")
let display_none = Cmd.(arg "-display" % "none")
let serial = function None -> "mon:stdio" | Some v -> v

(* Higher-level *)

let check_drive_image_file_type ~device image = match device with
| Smb | Virtio_9p_pci -> Os.Dir.must_exist image
| Nvme | Sata | Sata_cdrom | Usb | Usb_cdrom | Virtio_blk | Virtio_scsi
| Virtio_scsi_cdrom -> Os.File.must_exist image

let choose_drive ~use_virtio (device, image) =
  let* device = match device with
  | Some device ->
      let* () = check_drive_image_file_type ~device image in
      Ok device
  | None ->
      let* stat = Os.Path.stat image in
      match stat.Unix.st_kind with
      | Unix.S_DIR -> Ok Virtio_9p_pci
      | _ ->
          if Fpath.has_ext ".iso" image
          then (if use_virtio then Ok Virtio_scsi_cdrom else Ok Sata_cdrom)
          else (if use_virtio then Ok Virtio_blk else Ok Nvme)
  in
  let* image = Os.Path.realpath image in
  Ok (device, image)

let choose_drive_devices ~use_virtio drives =
  let choose d = choose_drive ~use_virtio d |> Result.error_to_failure in
  try match drives with
  | (None, image) :: drives when use_virtio ->
      (* TODO For windows the boot disk can't be specified as virtio-blk
         as the windows bootloader does has not the driver to access it. I
         don't think this is the case for Linux though we should devise
         something to avoid needless hardware emulation when it's not needed. *)
      Ok ((Nvme, image) :: List.map choose drives)
  | drives -> Ok (List.map choose drives)
  with
  | Failure e -> Error e

let cmd
    ~guest_arch ~accel ~mem_size ~use_virtio ~drives ~graphic ~use_ramfb
    ~use_usb_input
  =
  let host_os = Os.name () in
  let host_arch = Os.arch () in
  let* base =
    auto_base_cmd ~host_os ~host_arch ~guest_arch ~accel ~mem:mem_size
      ~smp:None
  in
  let* uefi_code = uefi_code ~host_os ~guest_arch in
  let uefi_drive = uefi_drive ~uefi_code in
  let nic =
    if use_virtio then Cmd.(arg "-nic" % nic_virtio_net_pci) else Cmd.empty
  in
  let device_usb_qemu_xhci = device_usb_qemu_xhci () in
  let usb_input_devices =
    if use_usb_input then usb_input_devices () else Cmd.empty
  in
  let drives = drives_cmd drives in
  let graphics =
    if use_ramfb then device_ramfb else
    if use_virtio then device_virtio_gpu_pci else Cmd.empty
  in
  let serial = serial None in
  let ui = if graphic then Cmd.empty else display_none in
  Ok Cmd.(base %% uefi_drive %% nic %% device_usb_qemu_xhci %%
          usb_input_devices %% drives %% graphics %% ui % "-serial" % serial)
