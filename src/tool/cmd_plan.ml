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

let check ~conf ~arch ~plan =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Telvm_conf.data_dir conf in
  let* _plan = get_plan ~data_dir ~plan in
  Log.stdout (fun m -> m "TODO");
  Ok 0

let script ~conf ~arch ~with_shutdown ~plan =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Telvm_conf.data_dir conf in
  let* plan = get_plan ~data_dir ~plan in
  let* script =
    Plan.to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan
  in
  print_string script;
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let envs = Telvm_cli.conf_envs

let plan =
  let doc = "The plan file to process" and docv = "[@]PLAN.ini" in
  Arg.(required & pos 0 (some B0_std_cli.filepath) None & info [] ~doc ~docv)

let check =
  let doc = "Check OS image build plan" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) checks if the image build plan is feasible." ]
  in
  Cmd.make (Cmd.info "check" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf
  and+ arch = Telvm_cli.arch_default_match_host ()
  and+ plan in
  check ~conf ~arch ~plan

let list = Telvm_cli.list_cmd Plans

let script =
  let doc = "Output cmd.exe OS image build script" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the build script of an image plan." ]
  in
  Cmd.make (Cmd.info "script" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf
  and+ arch = Telvm_cli.arch_default_match_host ()
  and+ plan
  and+ with_shutdown = Telvm_cli.with_shutdown in
  script ~conf ~arch ~with_shutdown ~plan

let cmd =
  let doc = "Operate on image plan files" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) operates on image plan files."; ]
  in
  Cmd.group (Cmd.info "plan" ~doc ~man ~envs) @@
  [check; script; list]
