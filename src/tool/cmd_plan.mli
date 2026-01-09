(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

val get_plan : data_dir:Fpath.t -> plan:Fpath.t -> (Plan.t, string) result
val cmd : int Cmdliner.Cmd.t
