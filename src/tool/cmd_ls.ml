(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let list_path ~dotfiles ~recurse ~rel ~follow_symlinks ~path =
  let output_path _stat _fname p () = print_endline (Fpath.to_string p) in
  let* is_dir = Os.Dir.exists path in
  if not is_dir
  then Ok (output_path () () (if rel then Fpath.basepath path else path) ())
  else Os.Dir.fold ~dotfiles ~rel ~recurse ~follow_symlinks output_path path ()

let list_image_path
    ~image ~mount_root ~dotfiles ~rel ~recurse ~follow_symlinks ~path
  =
  let output_path _stat _fname p () =
    let p =
      if rel then p else
      Disk_image.path_in_mount_to_image_path ~image ~mount_root p
    in
    print_endline (Fpath.to_string p)
  in
  let* mpath = Disk_image.get_existing_path_in_mount ~image ~mount_root path in
  let* is_dir = Os.Dir.exists mpath in
  if not is_dir
  then Ok (output_path () () (if rel then path else mpath) ())
  else Os.Dir.fold ~dotfiles ~rel ~recurse ~follow_symlinks output_path mpath ()

let ls ~config ~paths ~recurse ~follow_symlinks ~dotfiles ~full_path ~resolved =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let rel = not full_path in
  let list_path path =
    let p = Data_dir.resolve_path ~data_dir path in
    let* split = Disk_image.cut_file p in
    match split with
    | None ->
        let* exists = Os.Path.exists p in
        let* p = if resolved then Os.Path.realpath p else Ok p in
        if not exists then Fmt.error "%a: No such path" Fpath.pp path else
        list_path ~dotfiles ~rel ~recurse ~follow_symlinks ~path:p
    | Some (image, path) ->
        Disk_image.with_mount ~image @@ fun ~mount_root ->
        let image =
          if resolved then image else Data_dir.maybe_atify_path ~data_dir image
        in
        list_image_path
          ~image ~mount_root ~dotfiles ~rel ~recurse ~follow_symlinks ~path
  in
  let* () = List.iter_stop_on_error list_path paths in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "List directory contents, possibly in disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) list directories mostly like $(b,ls) would do. \
        A path to list can be inside a disk image if it is mountable by \
        the host operating system.";
    `Pre "$(cmd) $(b,/usr)\n\
          $(cmd) $(b,-R) $(b,/usr)\n\
          $(cmd) $(b,-Ra) $(b,@images/linux.img/usr)"; ]
  in
  Cmd.make (Cmd.info "ls" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ paths =
    let doc =
      "Path to list. Repeatable. $(i,IMAGE) is the path to \
       the disk image to mount in which $(i,PATH) is looked up. \
       If no image is specified $(i,PATH) is looked up on the host \
       file system."
    in
    Arg.(value & pos_all Tool_cli.image_path_conv [Fpath.v "."] &
         info [] ~doc)
  and+ recurse =
    let doc = "List paths recursively." in
    Arg.(value & flag & info ["R"; "recurse"] ~doc)
  and+ full_path =
    let doc = "Prepend the path to list in the output." in
    Arg.(value & flag & info ["p"; "full-path"] ~doc)
  and+ dotfiles =
    let doc = "List $(b,.) hidden paths." in
    Arg.(value & flag & info ["a"] ~doc)
  and+ follow_symlinks =
    let doc = "In directories, follow symlinks." in
    Arg.(value & flag & info ["L"] ~doc)
  and+ resolved = Tool_cli.resolved in
  ls ~config ~paths ~recurse ~follow_symlinks ~dotfiles ~full_path ~resolved
