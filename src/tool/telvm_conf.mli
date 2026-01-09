(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


(** [telvm] configuration. *)

open B0_std

type t
(** The type for configuration values. *)

val make : data_dir:Fpath.t -> state_dir:Fpath.t -> t
(** [make] is a configuration with given parameters. See accessors
    for semantics. *)

val discover :
  data_dir:Fpath.t option -> state_dir:Fpath.t option -> (t, string) result
(** [discover ~data_dir ~state_dir] is a configuration whose unspecified
    parameters are discovered. *)

val data_dir : t -> Fpath.t
(** [data_dir conf] is the data directory. May not exist. *)

val state_dir : t -> Fpath.t
(** [state_dir conf] is the state directory. May not exist. *)

val winvos_create_log_file : t -> Fpath.t
(** [winvos_create_log conf] is the path to the WinVOS create log file.
    May not exist. *)

val pp : t Fmt.t
(** [pp] formats configurations for inspection. *)
