(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let iter_archs archs f =
  let error = Log.if_error ~use:() in
  match List.iter_iter_on_error ~error (fun arch -> f ~arch) archs with
  | Error _ -> Ok Cmdliner.Cmd.Exit.some_error
  | Ok _ -> Ok 0

let iso_version_check ~data_dir ~arch =
  let* version = Winvos.installed_version ~data_dir ~arch in
  let* upstream = Winvos.upstream_version ~arch in
  let arch = Fmt.str "%a" Os.Arch.pp arch in
  match version with
  | None ->
      Fmt.pr "@[%6s iso: @[<v> %a@,upstream %a@]@]@."
        arch Fmt.(st [`Fg `Yellow]) "unavailable"
        Fmt.code upstream;
      Ok ()
  | Some version when String.equal version upstream ->
      Fmt.pr "@[%6s iso: @[version %a is %a@]@]@."
        arch Fmt.code version Fmt.(st [`Fg `Green]) "up-to-date";
      Ok ()
  | Some version ->
      Fmt.pr "@[%6s iso: @[<v> version %a is %a@,upstream %a@]@]@."
        arch Fmt.code version Fmt.(st [`Fg `Red]) "outdated"
        Fmt.code upstream;
      Ok ()

let make_plan_image ~data_dir ~script_src ~image_arch:arch ~dst =
  Log.stdout (fun m -> m "Preparing plan image");
  let* () =
    let byte_size = Byte_size.of_mb 500 in
    Disk_image.make
      ~partition_scheme:Gpt ~file_system:Exfat ~byte_size ~name:None
      ~force:true ~make_path:false dst
  in
  Disk_image.with_mount ~image:dst @@ fun ~mount_root ->
  let* () =
    let script_file = Fpath.(mount_root / "create-image.cmd") in
    Os.File.write ~force:false ~make_path:false script_file script_src
  in
  (*
  let* () =
    let dst = Fpath.(mount_root / "telvm") in
    Os.Dir.copy ~make_path (Fpath.v "telvm") ~dst
  in *)
  let* () =
    let* powershell_zip =
      Winvos.get_or_download_powershell_zip ~data_dir ~arch
    in
    Log.stdout (fun m ->
        m "Extracting %s in plan image" (Fpath.basename powershell_zip));
    let dst = Fpath.(mount_root / "powershell") in
    Os.Cmd.run Cmd.(tool "unzip" % "-q" % "-d" %%
                    path dst %% path powershell_zip)
  in
  Ok ()

let with_log_file file f =
  let flags = Unix.[O_WRONLY; O_CREAT; O_SHARE_DELETE; O_CLOEXEC; O_APPEND] in
  let force = true and make_path = true and atomic = false in
  Result.join @@ Os.File.write_with_fd ~force ~make_path ~atomic ~flags file f

let log_start fd =
  Os.Fd.write_string fd
    (Fmt.str "\x1c\n┌───imager──qemu─── %s\n" (Adhoc.now_rfc3339 ()))

let log_script_header ~image_arch fd =
  Os.Fd.write_string fd
    (Fmt.str "\n└───create-image.cmd──%a───┐\n" Os.Arch.pp image_arch)

let get_script_src_and_image_arch ~
    data_dir ~with_shutdown ~plan ~image_arch ~image
  =
  let image_arch ~plan_arch = match image_arch with
  | Some arch -> arch
  | None ->
      match plan_arch with
      | Some arch -> arch
      | None -> Adhoc.get_arch_of_file ~arch:None image
  in
  match plan with
  | None ->
      let plan = Plan.of_ini Winvos.Plan.base_src |> Result.get_ok in
      let arch = image_arch ~plan_arch:None in
      let* script =
        Plan.to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan
      in
      Ok (script, arch)
  | Some plan ->
      let plan = Data_dir.resolve_path ~data_dir plan in
      let* src = Os.File.read plan in
      if Fpath.has_ext ".cmd" plan
      then Ok (src, image_arch ~plan_arch:None) else
      let* plan = Plan.of_ini ~file:plan src in
      let arch = image_arch ~plan_arch:(plan.arch) in
      let* script_src =
        Plan.to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan
      in
      Ok (script_src, arch)

let create
    ~config ~dry_run ~accel ~mem_size ~with_shutdown ~plan ~imager ~imager_arch
    ~image_arch ~force ~image:image_unresolved
  =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let dur = Os.Mtime.counter () in
  let data_dir = Tool_config.data_dir config in
  let* image =
    let image = Data_dir.resolve_path ~data_dir image_unresolved in
    let* exists = Os.File.exists image in
    if exists && not force
    then Error (Tool_cli.err_file_exists_use_force image)
    else Ok image
  in
  let* imager, imager_arch = Winvos.Imager.get ~data_dir ~imager_arch imager in
  let* script_src, image_arch =
    get_script_src_and_image_arch
      ~data_dir ~with_shutdown ~plan ~image_arch ~image
  in
  Result.join @@ Os.File.with_tmp ~name:"plan-%s.img" @@ fun plan_image ->
  let* () = make_plan_image ~data_dir ~script_src ~image_arch ~dst:plan_image in
  let* virtio_win_iso = Winvos.get_or_download_virtio_win_iso ~data_dir in
  let* winvos_iso = Winvos.get_or_download_iso ~data_dir ~arch:image_arch in
  let* () =
    Log.stdout
      (fun m ->
         m "Extracting ValidationOS.vhdx from %s" (Fpath.basename winvos_iso));
    Disk_image.with_mount ~image:winvos_iso @@ fun ~mount_root:winvos_mount ->
    let force = true and make_path = true in
    Winvos.copy_winvos_vhdx ~force ~make_path ~winvos_mount ~dst:image
  in
  let images = [imager; image; plan_image; winvos_iso; virtio_win_iso] in
  let drives = List.map (fun p -> None, p) images in
  let* drives = Qemu.choose_drive_devices ~use_virtio:true drives in
  let* qemu =
    Qemu.cmd
      ~guest_arch:imager_arch ~accel ~mem_size ~drives ~use_virtio:true
      ~use_usb_input:true ~graphic:false ~use_ramfb:false
  in
  if dry_run
  then (Log.stdout (fun m -> m "%a" Adhoc.pp_dry_run qemu); Ok 0) else
  with_log_file (Tool_config.winvos_create_log_file config) @@ fun log_fd ->
  log_start log_fd;
  let* () = match Log.level () with
  | l when l > Warning -> Os.Cmd.run qemu
  | l ->
      let stop =
        if l = Log.Quiet then fun () -> () else
        Adhoc.Tui.spin ~msg:"Running imager" ()
      in
      Fun.protect ~finally:stop @@ fun () ->
      let null = Os.Cmd.in_null in
      let log = Os.Cmd.out_fd ~close:false log_fd in
      Os.Cmd.run ~stdin:null ~stdout:log ~stderr:log qemu
  in
  let* () =
    Disk_image.with_mount ~image:plan_image @@ fun ~mount_root ->
    let build_log = Fpath.(mount_root / "log.txt") in
    Os.File.read_with_fd build_log @@ fun src ->
    log_script_header ~image_arch log_fd;
    Os.Fd.copy src ~dst:log_fd
  in
  Log.stdout (fun m ->
      m "@[Image@ %a@ %a@ in@ %a@]"
        Fpath.pp image_unresolved
        (Fmt.st [`Fg `Green ]) "ready" Mtime.Span.pp (Os.Mtime.count dur));
  Ok 0

(* Commands *)

let bootstrap
    ~config ~accel ~mem_size ~build_script_file ~imager_arch ~force ~dst
  =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let bootstrap_arch = Winvos.Imager.get_bootstrap_arch () in
  let* build_script_src =
    Winvos.Imager.get_build_script_src
      ~data_dir ~bootstrap_arch ~imager_arch build_script_file
  in
  let* () =
    Winvos.Imager.make
      ~data_dir ~accel ~mem_size ~bootstrap_arch ~build_script_src
      ~imager_arch ~force ~make_path:true ~dst
  in
  Ok 0

let package_contents ~config ~arch ~names ~add_matching_wow64 =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* () = Winvos.Pkg.contents ~data_dir ~arch ~names ~add_matching_wow64 in
  Ok 0

let package_list ~config ~arch ~names ~add_matching_wow64 =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* () =
    let* pkgs = Winvos.Pkg.list ~data_dir ~arch ~names ~add_matching_wow64 in
    let pkgs = List.map Winvos.Pkg.name pkgs in
    Fmt.pr "@[<v>%a@]@." Fmt.(list string) pkgs;
    Ok ()
  in
  Ok 0

let log ~config ~no_pager ~op =
  (* XXX By using [B0_pager], we really depend on [b0.std] here perhaps we she
     should the module to [more] or [more.more] *)
  let warn_no_log log = Log.warn (fun m -> m "No %a file." Fpath.pp log) in
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let log = Tool_config.winvos_create_log_file config in
  let* exists = Os.File.exists log in
  let* () = match op with
  | `Delete ->
      if not exists
      then Ok (warn_no_log log)
      else Result.map ignore (Os.File.delete log)
  | `Output_path ->
      Ok (print_endline (Fpath.to_string log))
  | `Output_log | `Output_last as op ->
      let last = op = `Output_last in
      if not exists then Ok (warn_no_log log) else
      let* pager = B0_pager.find ~no_pager () in
      let* () = B0_pager.page_stdout pager in
      if last then begin
        let* log = Os.File.read log in
        let not_file_sep c = not (Char.equal '\x1c' c) in
        Ok (print_string (String.take_last_while not_file_sep log))
      end else begin
        Os.File.read_with_fd log @@ fun fd ->
        Os.Fd.copy fd ~dst:Unix.stdout
      end
  in
  Ok 0

let plan ~config ~arch ~which ~with_shutdown ~script =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* () = match which with
  | `Base ->
      let src = Winvos.Plan.base_src in
      let* src =
        if not script then Ok src else
        let* plan = Plan.of_ini src in
        Plan.to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan
      in
      print_string src; Ok ()
  | `Imager when script ->
      let* src =
        let bootstrap_arch = Winvos.Imager.get_bootstrap_arch () in
        Winvos.Imager.get_build_script_src ~data_dir
          ~bootstrap_arch ~imager_arch:arch None
      in
      print_string src; Ok ()
  | `Imager ->
      Log.err (fun m ->
          m "The imager is not a .plan file (yet). Use option %a."
            Fmt.code "--script");
      Ok ()
  in
  Ok 0

let reset ~config =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let telvm_dir = Data_dir.section_dir ~data_dir Telvm ~rel:false in
  Log.info (fun m -> m "Deleting %a" Fpath.pp telvm_dir);
  let* _existed = Os.Dir.delete ~recurse:true telvm_dir in
  Ok 0

let update ~config ~mode ~archs =
  let update_arch_iso ~data_dir ~force ~arch =
    let* httpc = B0_http.Http_client.make ~progress:true () in
    let* iso = Winvos.iso ~arch in
    let* exists = Data_file.exists ~data_dir iso in
    let* do_download =
      if force || not exists then Ok true else
      let* version = Winvos.installed_version ~data_dir ~arch in
      let* upstream = Winvos.upstream_version ~arch in
      Ok (not (String.equal (Option.get version) upstream))
    in
    if not do_download then Ok () else
    Result.map ignore (Data_file.download ~httpc ~data_dir iso)
  in
  let update_arch_powershell_zip ~data_dir ~force ~arch =
    let* httpc = B0_http.Http_client.make ~progress:true () in
    let* zip = Winvos.powershell_zip ~arch in
    let* exists = Data_file.exists ~data_dir zip in
    let* do_download =
      if force || not exists then Ok true else
      (* TODO check version ? *)
      Ok false
    in
    if not do_download then Ok () else
    Result.map ignore (Data_file.download ~httpc ~data_dir zip)
  in
  let update ~data_dir ~force ~arch =
    let* () = update_arch_iso ~data_dir ~force ~arch in
    update_arch_powershell_zip ~data_dir ~force ~arch
  in
  let version_check ~data_dir ~arch =
    let* () = iso_version_check ~data_dir ~arch in
    Ok ()
  in
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  match mode with
  | `Check -> iter_archs archs (version_check ~data_dir)
  | `Update -> iter_archs archs (update ~data_dir ~force:false)
  | `Force -> iter_archs archs (update ~data_dir ~force:true)

let version ~config ~archs ~mode =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let pr get_version ~arch =
    let* version = get_version ~arch in
    let arch = Fmt.str "%a" Os.Arch.pp arch in
    let pp_version = Fmt.(option ~none:(any "<none>") string) in
    Fmt.pr "@[%6s: %a@]@." arch pp_version version;
    Ok ()
  in
  match mode with
  | `Installed -> iter_archs archs (pr (Winvos.installed_version ~data_dir))
  | `Check -> iter_archs archs (iso_version_check ~data_dir)
  | `Upstream ->
      let upstream_version ~arch =
        Result.map Option.some (Winvos.upstream_version ~arch)
      in
      iter_archs archs (pr upstream_version)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let archs =
  let doc = "Architecture to consider. Repeatable." in
  let absent = "all supported architectures" in
  Arg.(value & opt_all Tool_cli.arch_conv Winvos.archs &
       info ["a"; "arch"] ~absent ~doc)

let plan =
  let doc = "Output built-in WinVOS OS plans" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs built-in WinVOS plans."; ]
  in
  Cmd.make (Cmd.info "plan" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ which =
    let builtin_plan_enum = ["base", `Base; "imager", `Imager] in
    let builtin_plan_conv = Arg.enum ~docv:"PLAN" builtin_plan_enum in
    let doc =
      Fmt.str "$(docv) is the built-in plan to output. Must be %s."
        (Arg.doc_alts_enum builtin_plan_enum)
    in
    Arg.(required & pos 0 (some builtin_plan_conv)  None &
         info [] ~doc)
  and+ arch = Tool_cli.arch_default_match_host ()
  and+ script =
    let doc = "Output the derived script rather than the $(b.plan) file." in
    Arg.(value & flag & info ["script"] ~doc)
  and+ with_shutdown = Tool_cli.with_shutdown in
  plan ~config ~arch ~which ~script ~with_shutdown

let bootstrap =
  let doc = "Create the imager needed by $(b,create)" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) creates the image used to create WinVOS images. \
        This is a WinVOS image itself."; ]
  in
  Cmd.make (Cmd.info "bootstrap" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ accel = Tool_cli.accel
  and+ mem_size = Tool_cli.mem_size
  and+ build_script_file =
    let doc =
      "Imager build script. The build script must end with a $(b,shutdown /p) \
       otherwise the user will have to type it in. If unspecified the \
       built-in script $(cmd.parent) $(b,plan --script imager) is used."
    in
    let absent = "built-in" in
    Arg.(value & opt (some Tool_cli.data_dir_path_conv) None &
         info ["script"] ~doc ~docv:"[@]SCRIPT.cmd" ~absent)
  and+ imager_arch =
    let doc = "$(docv) is the architecture of the imager OS." in
    let opts = ["imager-arch"] in
    Tool_cli.arch_default_match_host ~doc ~opts ()
  and+ force = Tool_cli.force
  and+ dst =
    let doc = "The destination file to write the image to." in
    let none = "@telvm/imager-$ARCH.img" in
    let docv = "[@]IMAGER.img" in
    Arg.(value & pos 0 (some ~none Tool_cli.data_dir_path_conv) None &
         info [] ~doc ~docv)
  in
  bootstrap ~config ~accel ~mem_size ~build_script_file ~imager_arch ~force ~dst

let create =
  let doc = "WinVOS image creation" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) creates a bootable WinVOS image. If no imager \
        is specified and the default imager cannot be found, \
        $(cmd.parent) $(b,bootstrap) is executed to create one (which may \
        in turn download the WinVOS data)." ]
  in
  Cmd.make (Cmd.info "create" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ accel = Tool_cli.accel
  and+ mem_size = Tool_cli.mem_size
  and+ dry_run = Tool_cli.dry_run
  and+ plan =
    let doc =
      "$(docv) is the image plan to use. If the file ends with $(b,.cmd) \
       the script is used as is for imaging and should end with a \
       $(b,shutdown /p) otherwise the invocation will not return. If \
       unspecified the built-in plan $(cmd.parent) $(b,plan base) is used."
    in
    let absent = "built-in" and docv = "[@]PLAN.[ini|cmd]" in
    Arg.(value & opt (some Tool_cli.data_dir_path_conv) None &
         info ["p"; "plan"] ~doc ~docv ~absent)
  and+ imager =
    let doc =
      "$(docv) is the imager to use. If no imager is specified and \
       no imager is found in $(b,@telvm) the bootstrap procedure is \
       performed as if $(cmd.parent) $(b,bootstrap) is invoked."
    in
    let none = "@telvm/winvos-imager-$HOST_ARCH.img" in
    let docv = "[@]IMAGER.img" in
    Arg.(value & opt (some ~none Tool_cli.data_dir_path_conv) None &
         info ["imager"] ~doc ~docv)
  and+ imager_arch =
    let absent = "See below" in
    let doc =
      "$(docv) is the architecture of the imager OS. If unspecified, \
       determined, in order from the imager file name or match the host \
       operating system."
    in
    let opts = ["imager-arch"] in
    Tool_cli.arch ~doc ~opts ~absent ()
  and+ image_arch =
    let absent = "See below" in
    let doc =
      "$(docv) is the architecture of the image OS. If unspecified, \
       determined, in order from the image file name, from the image plan \
       or match the host operating system."
    in
    let opts = ["image-arch"] in
    Tool_cli.arch ~doc ~opts ~absent ()
  and+ force = Tool_cli.force
  and+ image =
    let doc =
      "$(docv) is the path to the disk image to create. Note, for now only \
       NTFS vhdx disk images are generated."
    in
    let docv = "[@]IMAGE.vhdx" in
    Arg.(required & pos 0 (some Tool_cli.data_dir_path_conv) None &
         info [] ~doc ~docv)
  and+ with_shutdown = Tool_cli.with_shutdown in
  create
    ~config ~accel ~mem_size ~dry_run ~plan ~imager ~imager_arch ~image_arch
    ~force ~image ~with_shutdown

let log =
  let doc = "Output the WinVOS image creation log" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the image creation log." ]
  in
  let envs = B0_pager.Env.infos in
  Cmd.make (Cmd.info "log" ~doc ~man ~envs) @@
  let+ config = Tool_cli.config
  and+ no_pager = B0_pager.no_pager ()
  and+ op =
    let delete =
      let doc = "Delete the log file." in
      `Delete, Arg.info ["delete"] ~doc
    in
    let output_path =
      let doc = "Output path to the log file (may not exist)." in
      `Output_path, Arg.info ["path"] ~doc
    in
    let output_last =
      let doc = "Output only log of last image creation." in
      `Output_last, Arg.info ["l"; "last"] ~doc
    in
    Arg.(value & vflag `Output_log [delete; output_path; output_last])
  in
  log ~config ~no_pager ~op

let package =
  let doc = "Operate on WinVOS packages" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) operates on WinVOS cab packages."; ]
  in
  let config = Tool_cli.config in
  let arch = Tool_cli.arch_default_match_host () in
  let pkg_name =
    let complete ctx ~token = match ctx with
    | None -> Ok []
    | Some (conf, arch) ->
        let data_dir = Tool_config.data_dir conf in
        let* pkgs =
          Winvos.Pkg.list ~data_dir ~arch ~names:[] ~add_matching_wow64:false
        in
        let names = List.map Winvos.Pkg.name pkgs in
        let names = List.filter (String.starts_with ~prefix:token) names in
        Ok (List.map Arg.Completion.string names)
    in
    let context = Term.(const (fun x y -> x, y) $ config $ arch) in
    let completion = Arg.Completion.make ~context complete in
    Arg.Conv.of_conv ~completion Arg.string
  in
  let add_matching_wow64 =
    let doc = "Automatically list corresponding WOW64 package if there is one."
    in
    Arg.(value & flag & info ["W"; "add-matching-wow64"] ~doc)
  in
  let names =
    let doc =
      "$(cmd) package name to operate on. Repeatable. All if unspecified."
    in
    Arg.(value & pos_all pkg_name [] & info [] ~doc ~docv:"NAME")
  in
  let list =
    let doc = "List available WinVOS packages" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) lists available WinVOS cab packages."; ]
    in
    Cmd.make (Cmd.info "list" ~doc ~man) @@
    let+ config and+ arch and+ names and+ add_matching_wow64 in
    package_list ~config ~arch ~names ~add_matching_wow64
  in
  let contents =
    let doc = "List package contents of WinVOS packages" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) lists the contents of WinVOS packages."; ]
    in
    Cmd.make (Cmd.info "contents" ~doc ~man) @@
    let+ config and+ arch and+ names and+ add_matching_wow64 in
    package_contents ~config ~arch ~names ~add_matching_wow64
  in
  Cmd.group (Cmd.info "package" ~doc ~man) @@
  [list; contents]

let update =
  let doc = "Update WinVOS support files" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) updates, if needed, the WinVOS iso files and other support \
        files." ]
  in
  Cmd.make (Cmd.info "update" ~doc ~man) @@
  let+ config = Tool_cli.config and+ archs
  and+ mode =
    let check =
      Arg.info ["c"; "check"] ~doc:
        "Only check if there are new versions upstream. Do not download files."
    in
    let force =
      Arg.info ["f"; "force"] ~doc:"Force download regardless of freshness."
    in
    Arg.(value & vflag `Update [`Check, check; `Force, force])
  in
  update ~config ~mode ~archs

let reset =
  let doc = "Delete all WinVOS data" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) deletes the $(b,@telvm) directory"]
  in
  Cmd.make (Cmd.info "reset" ~doc ~man) @@
  let+ config = Tool_cli.config in
  reset ~config

let version =
  let doc = "Output WinVOS version of ISOs" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the version of WinVOS. This may not \
        be reliable is it munged from the download file name."; ]
  in
  Cmd.make (Cmd.info "version" ~doc ~man) @@
  let+ config = Tool_cli.config and+ archs
  and+ mode =
    let check =
      Arg.info ["c"; "check"] ~doc:
        "Check and report if there is a new version uptream (may be brittle)."
    in
    let upstream =
      Arg.info ["u"; "upstream"] ~doc:
        "Output the upstream version (may be brittle)."
    in
    Arg.(value & vflag `Installed [`Check, check; `Upstream, upstream])
  in
  version ~config ~archs ~mode

let cmd =
  let doc = "Windows Validation OS (WinVOS) support" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) Windows Validation OS (WinVOS) support"; ]
  in
  Cmd.group (Cmd.info "winvos" ~doc ~man) @@
  [bootstrap; create; log; package; plan; reset; update; version]
