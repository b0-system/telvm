(*---------------------------------------------------------------------------
   Copyright (c) 2026 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let div_round_up m n = (m + n - 1) / n (* Use Int.cdiv with OCaml >= 5.5 *)

(* Byte sizes *)

type t = int

let of_kb kb = kb * 1024
let of_mb mb = mb * 1024 * 1024
let of_gb gb = gb * 1024 * 1024 * 1024

let to_kb size = div_round_up size 1024
let to_mb size = div_round_up size (1024 * 1024)
let to_gb size = div_round_up size (1024 * 1024 * 1024)

let round_up_to_gb byte_size =
  of_gb (div_round_up byte_size (of_gb 1))
