(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let path ~conf ~paths =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Telvm_conf.data_dir conf in
  let resolve_path path = Data_dir.resolve_path ~data_dir path in
  Adhoc.print_paths (List.map resolve_path paths);
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Resolve paths like $(tool) does" in
  let envs = Telvm_cli.conf_envs in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) resolves paths as resolved by $(cmd.parent) \
        commands. The paths may not exist."; ]
  in
  Cmd.make (Cmd.info "path" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf
  and+ paths =
    let doc = "$(docv) is the path to resolve. Repeatable." in
    Arg.(value & pos_all Telvm_cli.data_dir_path_conv [] & info [] ~doc)
  in
  path ~conf ~paths
