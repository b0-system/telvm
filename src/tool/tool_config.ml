(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

type t = { data_dir : Filepath.t; state_dir : Filepath.t }

let make ~data_dir ~state_dir = { data_dir; state_dir }
let data_dir conf = conf.data_dir
let state_dir conf = conf.state_dir
let winvos_create_log_file conf = Filepath.(conf.state_dir / "winvos-create.log")
let pp = Fmt.record
    [ Fmt.field "data-dir" data_dir Filepath.pp;
      Fmt.field "state-dir" state_dir Filepath.pp ]

(* Discovery *)

let tooldir = "telvm"

let get_dir ~dir lookup_dir =
  let* dir = match dir with
  | Some dir -> Ok dir
  | None ->
      let* dir = lookup_dir () in
      Ok Filepath.(dir / tooldir)
  in
  let* exists = Os.Dir.exists dir in
  if not exists then Ok dir else Os.Path.realpath dir

let discover ~data_dir ~state_dir =
  let* data_dir = get_dir ~dir:data_dir Os.Dir.data in
  let* state_dir = get_dir ~dir:state_dir Os.Dir.state in
  Ok (make ~data_dir ~state_dir)
