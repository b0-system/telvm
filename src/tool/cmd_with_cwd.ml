(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let with_cwd ~config ~cwd ~tool ~tool_args =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let cmd = Cmd.list (tool :: tool_args) in
  let data_dir = Tool_config.data_dir config in
  let cwd = Data_dir.resolve_path ~data_dir cwd in
  let* split = Disk_image.cut_file cwd in
  let* () = match split with
  | None -> Os.Cmd.run ~cwd cmd
  | Some (image, cwd) ->
      Disk_image.with_mount ~image @@ fun ~mount_root ->
      let image = Data_dir.maybe_atify_path ~data_dir image in
      let* cwd = Disk_image.get_existing_path_in_mount ~image ~mount_root cwd in
      Os.Cmd.run ~cwd cmd
  in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cli_arg ~docv =
  let completion = Arg.Completion.complete_restart in
  Arg.Conv.of_conv ~docv Arg.string ~completion

let cmd =
  let doc = "Set cwd and execute host tool, possibly in disk images" in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(cmd) \
        [$(i,OPTION)]… $(i,[@][IMAGE/]PATH) $(b,--) $(i,TOOL) [$(i,ARG)]…";
    `S Manpage.s_description;
    `P "$(cmd) sets the current working directory and executes a tool.";
    `Pre "$(cmd) \
          $(b,@images/mydata.img/repos/ -- git clone ~/proj.git)\n\
          $(cmd) $(b,@images/mydata.img/ -- ls -al)\n\
          $(cmd) \
          $(b,@images/mydata.img/ -- tar -cvzf ~/mydata.img.tgz .)\n\
          $(cmd) $(b,@images/mydata.img/ -- \\$SHELL)" ]
  in
  Cmd.make (Cmd.info "with-cwd" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ cwd =
    let doc =
      "$(docv) is the current working directory to set. $(i,IMAGE) is the \
       path to the disk image to mount in which $(i,PATH) is looked up. \
       If no image is specified $(i,PATH) is looked up on the host \
       file system."
    in
    Arg.(required & pos 0 (some Tool_cli.image_path_conv) None & info [] ~doc)
  and+ tool =
    let doc = "Invoke tool $(docv)." in
    Arg.(required & pos 1 (some (cli_arg ~docv:"TOOL")) None & info [] ~doc)
  and+ tool_args =
    let doc =
      "Argument for the tool. Start with a $(b,--) token otherwise options \
       get interpreted by $(tool)."
    in
    Arg.(value & pos_right 1 (cli_arg ~docv:"ARG") [] & info [] ~doc)
  in
  with_cwd ~config ~cwd ~tool ~tool_args
