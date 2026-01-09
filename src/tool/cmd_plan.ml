(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let get_plan ~data_dir ~plan =
  let plan = Data_dir.resolve_path ~data_dir plan in
  let* plan_ini = Os.File.read plan in
  Plan.of_ini ~file:plan plan_ini

let check ~config ~arch ~plan =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* _plan = get_plan ~data_dir ~plan in
  Log.stdout (fun m -> m "TODO");
  Ok 0

let script ~config ~arch ~with_shutdown ~plan =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* plan = get_plan ~data_dir ~plan in
  let* script =
    Plan.to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan
  in
  print_string script;
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let plan =
  let doc = "The plan file to process" and docv = "[@]PLAN.ini" in
  Arg.(required & pos 0 (some B0_std_cli.filepath) None & info [] ~doc ~docv)

let check =
  let doc = "Check OS image build plan" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) checks if the image build plan is feasible." ]
  in
  Cmd.make (Cmd.info "check" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ arch = Tool_cli.arch_default_match_host ()
  and+ plan in
  check ~config ~arch ~plan

let list = Tool_cli.list_cmd Plans

let script =
  let doc = "Output cmd.exe OS image build script" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the build script of an image plan." ]
  in
  Cmd.make (Cmd.info "script" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ arch = Tool_cli.arch_default_match_host ()
  and+ plan
  and+ with_shutdown = Tool_cli.with_shutdown in
  script ~config ~arch ~with_shutdown ~plan

let cmd =
  let doc = "Operate on image plan files" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) operates on image plan files."; ]
  in
  Cmd.group (Cmd.info "plan" ~doc ~man) @@
  [check; script; list]
