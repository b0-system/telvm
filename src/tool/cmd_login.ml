(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let login ~config =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  Error ("login is TODO")

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Login into a bootable image" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) is like $(cmd.parent) $(b,run) except it attempts to directly
        log you in to get a prompt on the OS."; ]
  in
  Cmd.make (Cmd.info "login" ~doc ~man) @@
  let+ config = Tool_cli.config in
  login ~config
