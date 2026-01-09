(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let path ~config ~paths =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let resolve_path path = Data_dir.resolve_path ~data_dir path in
  Adhoc.print_paths (List.map resolve_path paths);
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Resolve paths like $(tool) does" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) resolves paths as resolved by $(cmd.parent) \
        commands. The paths may not exist."; ]
  in
  Cmd.make (Cmd.info "path" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ paths =
    let doc = "$(docv) is the path to resolve. Repeatable." in
    Arg.(value & pos_all Tool_cli.data_dir_path_conv [] & info [] ~doc)
  in
  path ~config ~paths
