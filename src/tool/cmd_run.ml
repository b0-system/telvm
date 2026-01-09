(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let warn_arch_not_found arch =
  Log.warn @@ fun m ->
  m "No guest architecture found in image names, using %a" Os.Arch.pp arch

let warn_multiple_arch arch =
  Log.warn @@ fun m ->
  m "Multiple guest architectures found in image names, using %a"
    Os.Arch.pp arch

let get_guest_arch ~guest_arch ~drives = match guest_arch with
| Some arch -> arch
| None ->
    let scrape_path (_, p) = Adhoc.scrape_arch_of_filepath p in
    let archs = List.filter_map scrape_path drives in
    match List.distinct Os.Arch.compare archs with
    | [arch] -> arch
    | arch :: _ -> warn_multiple_arch arch; arch
    | [] -> let arch = Os.arch () in warn_arch_not_found arch; arch

let run
    ~config ~guest_arch ~accel ~mem_size ~dry_run ~drives ~graphic
    ~use_ramfb ~use_virtio ~use_usb_input
  =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let resolve_path (dev, p) = (dev, Data_dir.resolve_path ~data_dir p) in
  let drives = List.map resolve_path drives in
  let* drives = Qemu.choose_drive_devices ~use_virtio drives in
  let* cmd =
    let guest_arch = get_guest_arch ~guest_arch ~drives in
    Qemu.cmd
      ~guest_arch ~accel ~mem_size ~drives ~graphic
      ~use_ramfb ~use_virtio ~use_usb_input
  in
  let* () =
    if dry_run
    then Ok (Log.stdout (fun m -> m "%a" Adhoc.pp_dry_run cmd)) else
    (* If the `cwd` is a symlink we get in the trouble with smb devices
       So we realpath the cwd. TODO do that in the Qemu module *)
    let* cwd = Os.Dir.cwd () in
    let* cwd = Os.Path.realpath cwd in
    Os.Cmd.run ~cwd cmd
  in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Run a bootable image" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) runs a bootable image with attached disk images. These other \
        disk images can also be picked up for booting. The bootloader \
        consider them for booting in order given on the command line.";
    `P "Use $(b,--dry-run) to see the underlying QEMU invocation. To quit \
        abruptly use $(b,C-a x) because $(b,C-c) is redirected to the \
        running OS. To quit less abruptly use $(b,C-a c) to enter the QEMU \
        monitor and issue $(b,system_powerdown) (may not work on ARM).";
    `P "If you use $(cmd) to install an ISO on a blank disk image specify \
        the ISO in second position:";
    `Pre "$(cmd) $(b,blank.qcow2 sys.iso)";
    `P "That way when you reboot after install, the disk image is \
        picked up for boot rather than the ISO." ]
  in
  Cmd.make (Cmd.info "run" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ dry_run = Tool_cli.dry_run
  and+ guest_arch =
    let absent = "Inferred from $(i,IMAGE) or match host OS arch" in
    Tool_cli.arch ~absent ()
  and+ accel = Tool_cli.accel
  and+ mem_size = Tool_cli.mem_size
  and+ graphic =
    let doc = "Enable graphic output." in
    Arg.(value & flag & info ["g"; "graphic"] ~doc)
  and+ use_ramfb =
    let doc = "Use $(b,-device ramfb)." in
    Arg.(value & flag & info ["use-ramfb"] ~doc)
  and+ drives =
    let doc =
      "$(docv) is a disk image exposed as a disk drive in the run. \
       Repeatable. The command line order specifies the boot order. \
       $(i,DEVICE) specifies the kind of drive for $(i,IMAGE). Must be one of \
       $(b,nvme), $(b,sata), $(b,sata-cdrom), $(b,usb), $(b,usb-cdrom), \
       $(b,virtio-blk), $(b,virtio-scsi), $(b,virtio-scsi-cdrom). If \
       unspecified, $(b,virtio-blk) is used and $(b,virtio-scsi-cdrom) \
       for $(b,.iso) files unless $(b,--no-virtio) is specified in which \
       case it is $(b,nvme) and $(b,sata-cdrom) respectively. $(b,TODO) \
       For now an unspecified device for the first drive results in an \
       $(b,nvme) device because the Windows bootloader can't see VirtIO \
       devices. In case a directory $(i,DIR) is specified direct host sharing \
       is enabled which can be used with $(b,virtio-9p-pci) (default) or \
       $(b,smb)."
    in
    Arg.(non_empty & pos_all Tool_cli.drive_device_and_data_dir_path_conv [] &
         info [] ~doc)
  and+ no_virtio =
    let doc =
      "Do not use VirtIO devices. Typically if the bootloader or guest lacks \
       VirtIO drivers. Affects the default $(i,DEVICE) for images \
       and the GPU."
    in
    Arg.(value & flag & info ["no-virtio"] ~doc)
  and+ no_usb_input =
    let doc =
      "Do not use USB input devices. Typically if the guest OS has \
       no USB driver."
    in
    Arg.(value & flag & info ["no-usb-input"] ~doc)
  in
  run ~config ~guest_arch ~drives ~accel ~mem_size ~dry_run ~graphic
    ~use_ramfb ~use_virtio:(not no_virtio)
    ~use_usb_input:(not no_usb_input)
