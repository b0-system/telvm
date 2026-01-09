(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner

let version = "%%VERSION%%"

let cmd =
  let doc = "Build and run OS images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) gets you prompts on QEMU virtual machines from your Unix \
        terminal. In particular it has support for building and getting \
        prompts on Windows 11 via Windows Validation OS (WinVOS).";
    `Pre "$(cmd) $(b,winvos update)\n\
          $(cmd) $(b,winvos create @boot/winvos.img)\n\
          $(cmd) $(b,run @boot/winvos.img)\n\
          $(cmd) $(b,run @boot/winvos.img -g) # If you still want the GUI\n\
          $(cmd) $(b,login @boot/winvos.img)  # May succeed to log you in"]
  in
  Cmd.group (Cmd.info "telvm" ~version:"%%VERSION%%" ~doc ~man) @@
  [Cmd_config.cmd; Cmd_cp.cmd; Cmd_image.cmd; Cmd_list.cmd;
   Cmd_login.cmd; Cmd_ls.cmd; Cmd_mv.cmd; Cmd_plan.cmd; Cmd_path.cmd;
   Cmd_rm.cmd; Cmd_run.cmd; Cmd_winvos.cmd; Cmd_with_cwd.cmd]

let main () =
  Log.time (fun _ m -> m "total time telvm %s" version) @@ fun () ->
  Cmd.eval' cmd

let () = if !Sys.interactive then () else exit (main ())
