(*---------------------------------------------------------------------------
   Copyright (c) 2026 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1:byte_size Bytes sizes} *)

type t = int
(** The type for file system sizes. *)

val of_kb : int -> t
(** [of_kb n] is [n] KB in bytes. *)

val of_mb : int -> t
(** [of_mb n] is [n] MB in bytes. *)

val of_gb : int -> t
(** [of_gb n] is [n] GB in bytes. *)

val to_kb : int -> t
(** [to_kb size] is [size] in KB (rounds up). *)

val to_mb : int -> t
(** [to_mb size] is [size] in MB (rounds up). *)

val to_gb : int -> t
(** [to_gb size] is [size] in GB (rounds up). *)

val round_up_to_gb : t -> t
(** [round_up_to_gb size] rounds [n] bytes up to the neareast
    full GB. *)
