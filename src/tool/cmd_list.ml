(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let cmd =
  Tool_cli.list_cmd ~doc:"List images of the $(b,@boot) directory" Boot
