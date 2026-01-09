(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let err_is_dir p = Fmt.str "%a is a directory (not copied)" Fpath.pp p

let cp_src_file ~force ~src ~src_base ~dst_stat ~dst = match dst_stat with
| None | Some { Unix.st_kind = S_REG } ->
    Os.File.copy ~force ~make_path:true src ~dst
| Some { Unix.st_kind = S_DIR } ->
    let dst = Fpath.(dst / src_base) in
    Os.File.copy ~force ~make_path:false src ~dst
| Some _ ->
    Error (Tool_cli.err_not_file_or_dir dst)

let rec cp_src_dir
    ~force ~follow_symlinks ~src ~src_base ~content ~dst_stat ~dst
  =
  match dst_stat with
  | None ->
      Os.Dir.copy ~follow_symlinks ~make_path:true src ~dst
  | Some { Unix.st_kind = S_DIR } ->
      if not content then
        let dst = Fpath.(dst / src_base) in
        Os.Dir.copy ~follow_symlinks ~make_path:true src ~dst
      else if src_base = ".fseventsd" (* macOS kludge *) then
        Ok (Log.warn (fun m -> m "Skipping directory %a" Fpath.pp src))
      else
      let cp src =
        let src_base = Fpath.basename src in
        let recurse = true and content = false in
        cp_with_src
          ~force ~recurse ~follow_symlinks ~src ~src_base ~content ~dst_stat
          ~dst
      in
      let* contents =
        Os.Dir.contents ~dotfiles:true ~follow_symlinks ~recurse:false src
      in
      List.iter_stop_on_error cp contents
  | Some { Unix.st_kind = S_REG } ->
      Error (Tool_cli.err_not_dir dst)
  | Some _ ->
      Error (Tool_cli.err_not_file_or_dir dst)

and cp_with_src
    ~force ~recurse ~follow_symlinks ~src ~src_base ~content ~dst_stat ~dst
  =
  let* src_stat =
    if Fpath.is_dash src then Ok None else Os.Path.exists_stat src
  in
  match src_stat with
  | Some { Unix.st_kind = S_REG } ->
      cp_src_file ~force ~src ~src_base ~dst_stat ~dst
  | None when Fpath.is_dash src ->
      cp_src_file ~force ~src ~src_base ~dst_stat ~dst
  | Some { Unix.st_kind = S_DIR } ->
      if not recurse then Error (err_is_dir src) else
      cp_src_dir ~force ~follow_symlinks ~src ~src_base ~content ~dst_stat ~dst
  | Some _ -> Error (Tool_cli.err_not_file_or_dir src)
  | None -> Error (Tool_cli.err_no_such_file_or_dir src)

let cp_src
    ~data_dir ~force ~recurse ~follow_symlinks ~src ~content
    ~dst_mount ~dst_stat ~dst
  =
  let src = Data_dir.resolve_path ~data_dir src in
  let* split = Disk_image.cut_file src in
  let* () = match split with
  | None ->
      let src_base = Fpath.basename src in
      cp_with_src
        ~force ~recurse ~follow_symlinks ~src ~src_base ~content ~dst_stat ~dst
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
      let src_base =
        if Fpath.is_root src then Fpath.basename src_image else
        Fpath.basename src
      in
      let* src =
        let image = Data_dir.maybe_atify_path ~data_dir src_image in
        Disk_image.get_existing_path_in_mount ~image ~mount_root src
      in
      cp_with_src
        ~force ~recurse ~follow_symlinks ~src ~src_base ~content ~dst_stat ~dst
  in
  Ok ()

let cp_with_dst
    ~data_dir ~force ~recurse ~follow_symlinks ~srcs ~content ~dst_mount ~dst =
  let* dst_stat =
    if Fpath.is_dash dst then Ok None else Os.Path.exists_stat dst
  in
  match srcs with
  | [] -> assert false
  | [src] ->
      cp_src ~data_dir ~force ~recurse ~follow_symlinks ~src ~content
        ~dst_mount ~dst_stat ~dst
  | srcs ->
      match dst_stat with
      | Some { Unix.st_kind = S_DIR } ->
          let cp src =
            cp_src ~data_dir ~force ~recurse ~follow_symlinks ~src ~content
              ~dst_mount ~dst_stat ~dst
          in
          List.iter_stop_on_error cp srcs
      | _ -> Error (Tool_cli.err_not_dir dst)

let cp_cmd ~config ~srcs ~content ~dst ~force ~recurse ~follow_symlinks =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let dst = Data_dir.resolve_path ~data_dir dst in
  let* split = Disk_image.cut_file dst in
  let* () = match split with
  | None ->
      cp_with_dst
        ~data_dir ~force ~recurse ~follow_symlinks ~srcs ~content
        ~dst_mount:None ~dst
  | Some (image, dst) ->
      let* dst_image_realpath = Os.Path.realpath image in
      Disk_image.with_mount ~image @@ fun ~mount_root ->
      let dst_mount = Some (dst_image_realpath, mount_root) in
      let image = Data_dir.maybe_atify_path ~data_dir image in
      let* dst =
        Disk_image.get_syntactic_path_in_mount ~image ~mount_root dst
      in
      cp_with_dst ~data_dir ~force ~recurse ~follow_symlinks ~srcs ~content
        ~dst_mount ~dst
  in
  Ok 0

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Copy files and directories, possibly in disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) copies files or directories mostly like $(b,cp) would do.";
    `P "If $(i,DST) is a directory the sources are copied therein. \
        If there are multiple sources the last argument must be a \
        directory. Sources and destinations can be inside disk \
        images but these must mountable by your host operating system
        which \ may not always be the case.";
    `Pre
      "$(cmd) $(b,-r image.iso/ image-contents)\n\
       $(cmd) $(b,-rc image.iso/ image.img/)\n\
       $(cmd) $(b,image.iso @images/image.iso)\n\
       $(cmd) $(b,-r this that image.img/)\n\n\
       $(cmd) $(b,- image.img/README.md < README.md)\n\
       $(cmd) $(b,image.img/README.md -)\n\
       "]
  in
  Cmd.make (Cmd.info "cp" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ srcs =
    let doc =
      "Source path to copy. Repeatable. $(i,IMAGE) is the path to a disk
       image to mount in which $(i,SRC) is looked up. If no image is \
       specified $(i,SRC) is looked up on the host file system."
    in
    let docv = "[@][IMAGE/]SRC" in
    Arg.(non_empty & pos_left ~rev:true 0 Tool_cli.image_path_conv [] &
         info [] ~doc ~docv)
  and+ dst =
    let doc =
      "Destination path of the copy. If the destination is an existing \
       directory sources are copied therein. Must be a directory \
       if there is more than one source. $(i,IMAGE) is the path \
       to a disk image to mount in which $(i,DST) is looked up. If no image \
       is specified $(i,DST) is looked up on the host file system."
    in
    let docv = "[@][IMAGE/]DST" in
    Arg.(required & pos ~rev:true 0 (some Tool_cli.image_path_conv) None &
         info [] ~doc ~docv)
  and+ force =
    let doc = "Force copy if destination is a file and already exists." in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  and+ recurse =
    let doc = "Copy directories recursively." in
    Arg.(value & flag & info ["R"; "r"; "recurse"] ~doc)
  and+ follow_symlinks =
    let doc = "In directories, follow symlinks instead of copying links." in
    Arg.(value & flag & info ["L"] ~doc)
  and+ content =
    let doc =
      "If $(b,-r) is specified and the destination is an existing directory, \
       copy the contents of source directories to the destination (instead of \
       creating directories inside the directory). $(b,.fseventsd) directories
       in sources are not copied over (macOS kludge)."
    in
    Arg.(value & flag & info ["c"; "content"] ~doc)
  in
  cp_cmd ~config ~srcs ~content ~dst ~force ~recurse ~follow_symlinks
