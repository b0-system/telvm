(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let warn_no_path p =
  Log.warn (fun m -> m "%a: No such file or directory" Fpath.pp p)

let warn_image_empty ~image =
  Log.warn (fun m -> m "%a: Image is empty" Fpath.pp image)

let err_no_recurse ~image =
  Fmt.str "%a: Use option %a to delete the image contents"
    Fpath.pp image Fmt.code "-r"

let delete_path ~recurse path =
  let* existed = Os.Path.delete ~recurse path in
  if not existed then warn_no_path path;
  Ok ()

let delete_image_root ~image ~recurse path =
  if not recurse then Error (err_no_recurse ~image) else
  let* root_contents =
    Os.Dir.contents ~dotfiles:true ~follow_symlinks:false ~recurse:false path
  in
  if root_contents = [] then Ok (warn_image_empty ~image) else
  let error e = Log.if_error ~level:Warning ~use:() e in
  let delete p = Result.map ignore (Os.Path.delete ~recurse:true p) in
  ignore (List.iter_iter_on_error ~error delete root_contents);
  Ok ()

let rm_path ~data_dir ~recurse path =
  let path = Data_dir.resolve_path ~data_dir path in
  let* split = Disk_image.cut_file path in
  match split with
  | None -> delete_path ~recurse path
  | Some (image, path) ->
      Disk_image.with_mount ~image @@ fun ~mount_root ->
      let image = Data_dir.maybe_atify_path ~data_dir image in
      let* path =
        Disk_image.get_existing_path_in_mount ~image ~mount_root path
      in
      if Fpath.equal path mount_root
      then delete_image_root ~image ~recurse path
      else delete_path ~recurse path

let rm ~config ~paths ~recurse =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* () = List.iter_stop_on_error (rm_path ~data_dir ~recurse) paths in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Delete paths, possibly in disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) delete files or directories mostly like $(b,rm) would do. \
      y  Paths to delete can be inside disk images if they are mountable by \
        the host operating system.";
    `Pre "$(cmd) $(b,-r trash)\n\
          $(cmd) $(b,-r image.iso/trash)\n\
          $(cmd) $(b,-r @images/data.img/trash)";]
  in
  Cmd.make (Cmd.info "rm" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ paths =
    let doc =
      "Path to delete. Repeatable. $(i,IMAGE) is the path to \
       the disk image to mount in which $(i,PATH) is looked up. \
       If no image is specified $(i,PATH) is looked up on the host \
       file system."
    in
    Arg.(non_empty & pos_all Tool_cli.image_path_conv [] & info [] ~doc)
  and+ recurse =
    let doc = "Delete paths recursively." in
    Arg.(value & flag & info ["r"; "R"; "recurse"] ~doc)
  in
  rm ~config ~paths ~recurse
