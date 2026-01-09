(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let err_image_root p = Fmt.str "%a: Cannot move image root" Fpath.pp p

let mv_via_copy_and_rm ~force ~src ~src_base ~dst_stat ~dst =
  let* () =
    Cmd_cp.cp_with_src
      ~force ~recurse:true ~follow_symlinks:false ~src ~src_base ~content:false
      ~dst_stat ~dst
  in
  Result.map ignore (Os.Path.delete ~recurse:true src)

let rec find_dst_dev dst =
  if Fpath.is_root dst then Error "Could not find destination device" else
  match Os.Path.exists_stat dst with
  | Error _ as e -> e
  | Ok None -> find_dst_dev (Fpath.parent dst)
  | Ok (Some { st_dev }) -> Ok st_dev

let mv_with_src ~force ~src ~src_base ~dst_stat ~dst =
  let* src_stat = Os.Path.exists_stat src in
  match src_stat with
  | Some ({ Unix.st_kind = S_REG | S_DIR} as src_stat) ->
      begin match dst_stat with
      | Some { Unix.st_kind = S_REG | S_DIR; st_dev = dst_dev }
        when not (src_stat.st_dev = dst_dev) ->
          mv_via_copy_and_rm ~force ~src ~src_base ~dst_stat ~dst
      | Some { Unix.st_kind = S_REG } ->
          Os.Path.rename ~force ~make_path:false src ~dst
      | Some { Unix.st_kind = S_DIR; st_dev = dst_dev } ->
          Os.Path.rename ~force ~make_path:false src ~dst
      | None ->
          let* dst_dev = find_dst_dev dst in
          if src_stat.st_dev = dst_dev
          then Os.Path.rename ~force ~make_path:true src ~dst
          else mv_via_copy_and_rm ~force ~src ~src_base ~dst_stat ~dst
      | Some _ ->
          Error (Tool_cli.err_not_file_or_dir dst)
      end
  | Some _ -> Error (Tool_cli.err_not_file_or_dir src)
  | None -> Error (Tool_cli.err_no_such_file_or_dir src)

let mv_src ~data_dir ~force ~src ~dst_mount ~dst_stat ~dst =
  let src = Data_dir.resolve_path ~data_dir src in
  let* split = Disk_image.cut_file src in
  let* () = match split with
  | None ->
      let src_base = Fpath.basename src in
      mv_with_src ~force ~src ~src_base ~dst_stat ~dst
  | Some (src_image, src) ->
      let src_image_with_mount ~dst_mount ~src_image f =
        (* We can't mount twice *)
        match dst_mount with
        | None -> Disk_image.with_mount ~image:src_image f
        | Some (dst_image_realpath, dst_mount) ->
            let* src_image_realpath = Os.Path.realpath src_image in
            if Fpath.equal dst_image_realpath src_image_realpath
            then f ~mount_root:dst_mount
            else Disk_image.with_mount ~image:src_image f
      in
      src_image_with_mount ~dst_mount ~src_image @@ fun ~mount_root ->
      let* src_base =
        if Fpath.is_root src
        then Error (err_image_root Fpath.(src_image // src))
        else Ok (Fpath.basename src)
      in
      let* src =
        let image = Data_dir.maybe_atify_path ~data_dir src_image in
        Disk_image.get_existing_path_in_mount ~image ~mount_root src
      in
      mv_with_src ~force ~src ~src_base ~dst_stat ~dst
  in
  Ok ()

let mv_with_dst ~data_dir ~force ~srcs ~dst_mount ~dst =
  let* dst_stat = Os.Path.exists_stat dst in
  match srcs with
  | [] -> assert false
  | [src] -> mv_src ~data_dir ~force ~src ~dst_mount ~dst_stat ~dst
  | srcs ->
      match dst_stat with
      | Some { Unix.st_kind = S_DIR } ->
          let mv src = mv_src ~data_dir ~force ~src ~dst_mount ~dst_stat ~dst in
          List.iter_stop_on_error mv srcs
      | _ ->
          Error (Tool_cli.err_not_dir dst)

let mv_cmd ~config ~force ~srcs ~dst =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let dst = Data_dir.resolve_path ~data_dir dst in
  let* split = Disk_image.cut_file dst in
  let* () = match split with
  | None -> mv_with_dst ~data_dir ~force ~srcs ~dst_mount:None ~dst
  | Some (image, dst) ->
      let* dst_image_realpath = Os.Path.realpath image in
      Disk_image.with_mount ~image @@ fun ~mount_root ->
      let dst_mount = Some (dst_image_realpath, mount_root) in
      let image = Data_dir.maybe_atify_path ~data_dir image in
      let* dst =
        Disk_image.get_syntactic_path_in_mount ~image ~mount_root dst
      in
      mv_with_dst ~data_dir ~force ~srcs ~dst_mount ~dst
  in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Move files, possibly in disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) move files or directories mostly like $(b,mv) would do. \
        Conceptually works like $(cmd.parent) $(b,cp) followed by \
        a $(cmd.parent) $(b,rm) of the sources."]
  in
  Cmd.make (Cmd.info "mv" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ srcs =
    let doc =
      "Source path to move. Repeatable. $(i,IMAGE) is the path to a disk
       image to mount in which $(i,SRC) is looked up. If no image is \
       specified $(i,SRC) is looked up on the host file system."
    in
    let docv = "[@][IMAGE/]SRC" in
    Arg.(non_empty & pos_left ~rev:true 0 Tool_cli.image_path_conv [] &
         info [] ~doc ~docv)
  and+ dst =
    let doc =
      "Destination path of the move. If the destination is an existing \
       directory sources are moved therein. Must be a directory \
       if there is more than one source. $(i,IMAGE) is the path \
       to a disk image to mount in which $(i,DST) is looked up. If no image \
       is specified $(i,DST) is looked up on the host file system."
    in
    let docv = "[@][IMAGE/]DST" in
    Arg.(required & pos ~rev:true 0 (some Tool_cli.image_path_conv) None &
         info [] ~doc ~docv)
  and+ force =
    let doc = "Force move if destination is a file and already exists." in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  in
  mv_cmd ~config ~force ~srcs ~dst
