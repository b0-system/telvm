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

let accel ~host_os ~host_arch ~guest_arch =
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

let base_cmd ~system ~machine ~accel ~cpu ~smp ~mem =
  Cmd.(system % "-machine" % machine % "-accel" % accel % "-cpu" % cpu %
       "-smp" % smp % "-m" % mem)

let auto_base_cmd ~host_os ~host_arch ~guest_arch ~smp:s ~mem:m =
  let* system = system ~guest_arch in
  let* machine = machine ~guest_arch in
  let accel = accel ~host_os ~host_arch ~guest_arch in
  let* cpu = cpu ~host_arch ~guest_arch in
  let smp = smp s in
  let mem = mem m in
  Ok (base_cmd ~system ~machine ~accel ~cpu ~smp ~mem)

(* Bootloader parameters *)

(* For now we eschew that the nvram I guess it's useful to persist bootloader
   params but are we interested ? Note that we'd need one per vm run
   let efi_vars_drive =
     Fmt.str "if=pflash,format=raw,file=%s" "edk2-i386-vars.fd"  *)

let uefi_code ~host_os ~guest_arch =
  let err_host_os name =
    Fmt.error "%a: Don't know where the UEFI code is located"
      Os.Name.pp_id name
  in
  let err_guest_arch arch =
    Fmt.error "%a guest: Don't know which UEFI code to use" Os.Arch.pp_id arch
  in
  (* TODO it's a bit sad these things don't seem to be looked up
     in relative mode (or is the way we specify uefi code wrong ?
     Should we munge the '-L help' output and lookup ourselves ?
     Seems less brittle than these hard-coded paths. *)
  let* file = match guest_arch with
  | Os.Arch.Arm64 _ ->
      begin match host_os with
      | Os.Name.Darwin _ -> (* TODO macports *)
          Ok "/opt/homebrew/share/qemu/edk2-aarch64-code.fd"
      | Os.Name.Linux _ ->
          Fmt.error "TODO"
      | name -> err_host_os name
      end
  | Os.Arch.X86_64 _ ->
      begin match host_os with
      | Os.Name.Darwin _ -> (* TODO macports *)
          Ok "/opt/homebrew/share/qemu/edk2-x86_64-code.fd"
      | Os.Name.Linux _ ->
          Fmt.error "TODO"
      | name -> err_host_os name
      end
  | arch -> err_guest_arch arch
  in
  let file = Fpath.v file in
  let* () = Os.File.must_exist file in
  Ok file

let uefi_drive ~uefi_code =
  let code = Fpath.to_string uefi_code in
  let drive = Fmt.str "if=pflash,format=raw,readonly=on,file=%s" code in
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
| Nvme | Sata | Sata_cdrom | Usb | Usb_cdrom | Virtio_blk | Virtio_scsi
| Virtio_scsi_cdrom

let drive_devices =
  [ Nvme; Sata; Sata_cdrom; Usb; Usb_cdrom; Virtio_blk; Virtio_scsi;
    Virtio_scsi_cdrom ]

let drive_device_to_string = function
| Nvme -> "nmve"
| Sata -> "sata"
| Sata_cdrom -> "sata-cdrom"
| Usb -> "usb"
| Usb_cdrom -> "usb-cdrom"
| Virtio_blk -> "virtio-blk"
| Virtio_scsi -> "virtio-scsi"
| Virtio_scsi_cdrom -> "virtio-scsi-cdrom"

let drive_device_of_string = function
| "nvme" -> Ok Nvme
| "sata" -> Ok Sata
| "sata-cdrom" -> Ok Sata_cdrom
| "usb" -> Ok Usb
| "usb-cdrom" -> Ok Usb_cdrom
| "virtio-blk" -> Ok Virtio_blk
| "virtio-scsi" -> Ok Virtio_scsi
| "virtio-scsi-cdrom" -> Ok Virtio_scsi_cdrom
| kind ->
    Fmt.error "%s: unknown drive device. %a"
      kind Fmt.(must_be code) (List.map drive_device_to_string drive_devices)

type drive = drive_device * Fpath.t

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

let cdrom_drive ~driveid ~file =
  Fmt.str "if=none,id=%s,media=cdrom,format=raw,file=%a" driveid Fpath.pp file

let drive ?(cache = "") ~driveid ~file () =
  let cache = if cache = "" then "" else Fmt.str "cache=%s," cache in
  let format = if Fpath.has_ext ".img" file then "format=raw," else "" in
  Fmt.str "if=none,id=%s,%s%sfile=%a" driveid cache format Fpath.pp file

let device ?(bus = "") ?(serial = "") ~driveid ~devid ~bootidx ~driver () =
  let serial = if serial = "" then "" else Fmt.str "serial=%s," serial in
  let bus = if bus = "" then "" else Fmt.str "bus=%s," bus in
  Fmt.str "drive=%s,id=%s,%sbootindex=%d,%sdriver=%s"
    driveid devid bus bootidx serial driver

let drives_cmd ds =
  let add_drive (id, cmd, add_scsi_controller) d =
    let driveid = Fmt.str "drive%02d" id in
    let devid = Fmt.str "dev%02d" id in
    let drive, device, add_scsi = match d with
    | Usb, image ->
        drive ~driveid ~file:image (),
        device ~driveid ~devid ~bootidx:id ~driver:"usb-storage" (),
        add_scsi_controller
    | Usb_cdrom, image ->
        cdrom_drive ~driveid ~file:image,
        device ~driveid ~devid ~bootidx:id ~driver:"usb-storage" (),
        add_scsi_controller
    | Virtio_blk, image ->
        drive ~driveid ~cache:"none" ~file:image (),
        device ~driveid ~devid ~bootidx:id ~driver:"virtio-blk" (),
        add_scsi_controller
    | Virtio_scsi, image ->
        drive ~driveid ~cache:"none" ~file:image (),
        device ~driveid ~devid ~bootidx:id ~driver:"scsi-hd" (),
        true
    | Virtio_scsi_cdrom, image ->
        cdrom_drive ~driveid ~file:image,
        device ~driveid ~devid ~bootidx:id ~driver:"scsi-cd" (),
        true
    | Sata, image ->
        drive ~driveid ~cache:"none" ~file:image (),
        device ~driveid ~devid ~bootidx:id ~serial:devid ~driver:"ide-hd" (),
        add_scsi_controller
    | Sata_cdrom, image ->
        let bus = Fmt.str "ide.%d" id in
        cdrom_drive ~driveid ~file:image,
        device ~driveid ~devid ~bus ~bootidx:id ~driver:"ide-cd" (),
        add_scsi_controller
    | Nvme, image ->
        drive ~driveid ~cache:"none" ~file:image (),
        device ~driveid ~devid ~bootidx:id ~serial:devid ~driver:"nvme" (),
        add_scsi_controller
    in
    id + 1, Cmd.(cmd % "-drive" % drive % "-device" % device), add_scsi
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

let choose_drive ~use_virtio (device, image) =
  let device = match device with
  | Some device -> device
  | None ->
      if Fpath.has_ext ".iso" image
      then (if use_virtio then Virtio_scsi_cdrom else Sata_cdrom)
      else (if use_virtio then Virtio_blk else Nvme)
  in
  (device, image)

let choose_drive_devices ~use_virtio drives =
  let mk_drive = choose_drive ~use_virtio in
  match drives with
  | (None, image) :: drives when use_virtio ->
      (* TODO For windows the boot disk can't be specified as virtio-blk
         as the windows bootloader does has not the driver to access it. I
         don't think this is the case for Linux though we should devise
         something to avoid needless hardware emulation when it's not needed. *)
      (Nvme, image) :: List.map mk_drive drives
  | drives -> List.map mk_drive drives

let cmd
    ~guest_arch ~mem_size ~use_virtio ~drives ~graphic ~use_ramfb ~use_usb_input
  =
  let host_os = Os.name () in
  let host_arch = Os.arch () in
  let* base =
    auto_base_cmd ~host_os ~host_arch ~guest_arch ~mem:mem_size ~smp:None
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
