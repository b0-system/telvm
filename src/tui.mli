(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type spin_scheme = string array * float
(** Array of frames and frame duration. *)

type stop = unit -> unit

val spin : ?scheme:spin_scheme -> msg:string -> unit -> stop
