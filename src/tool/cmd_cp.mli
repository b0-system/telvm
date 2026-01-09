(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

val cp_with_src :
  force:bool -> recurse:bool -> follow_symlinks:bool ->
  src:Filepath.t -> src_base:string -> content:bool ->
  dst_stat:Unix.stats option -> dst:Filepath.t -> (unit, string) result

val cmd : int Cmdliner.Cmd.t
