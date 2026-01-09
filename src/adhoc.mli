(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ad hoc module. *)

open B0_std

(** Ad hoc TUI spinners. *)
module Tui : sig

  type spin_scheme = string array * float
  (** Array of frames and frame duration. *)

  type stop = unit -> unit

  val spin : ?scheme:spin_scheme -> msg:string -> unit -> stop
end

val print_paths : Fpath.t list -> unit

val pp_dry_run : Cmd.t Fmt.t
(** Similar to [Cmd.pp_shell] but different. *)

val tool_version : Cmd.t -> (string option, string) result

val now_rfc3339  : unit -> string

(** {1:adhoc_munging Ad hoc munging logic} *)

val scrape_arch_of_filepath : Fpath.t -> Os.Arch.t option
(** [scrape_arch_of_filepath file] tries to determine an architecture
    from [file] by starting from the end of the path. *)

val get_arch_of_file : arch:Os.Arch.t option -> Fpath.t -> Os.Arch.t
(** [get_arch_of_file ~arch f] is [a] if [arch] is [Some a] otherwise
    tries {!scrape_arch_of_filepath} and it that is [None] falls back
    to {!Os.arch} (a warning is emitted). *)
