(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let data_dir ~conf ~delete =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Telvm_conf.data_dir conf in
  let* () =
    if not delete
    then Ok (Fmt.pr "%a@." Fpath.pp data_dir)
    else Result.map ignore (Os.Dir.delete ~recurse:true data_dir)
  in
  Ok 0

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Operate on the data directory of $(tool)" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the path to the data directory of $(tool)." ]
  in
  let envs = Telvm_cli.conf_envs in
  Cmd.make (Cmd.info "data-dir" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf
  and+ delete =
    let doc = "Delete data directory." in
    Arg.(value & flag & info ["delete"] ~doc)
  in
  data_dir ~conf ~delete
