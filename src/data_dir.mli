(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data directory management. *)

open B0_std

(** {1:path \@path handling} *)

val is_at_path : Fpath.t -> bool
(** [is_at_path p] is [true] iff [p] starts with [@]. *)

val atify_path : Fpath.t -> Fpath.t
(** [atify_path p] prefixes [p] with [@]. [p] is assumed to be relative. *)

val maybe_atify_path : data_dir:Fpath.t -> Fpath.t -> Fpath.t
(** [maybe_atify_path ~data_dir p] atifies [p] iff [data_dir]
    is a strict prefix of [p]. *)

val resolve_path : data_dir:Fpath.t -> Fpath.t -> Fpath.t
(** [resolve_path ~data_dir ~default p] is [p] if [p] does not start
    with [@], otherwise replaces [@] by the [data_dir] path. The path
    may not exist. *)

(** {1:file Data directory files} *)

val files : data_dir:Fpath.t -> atify:bool -> (Fpath.t list, string) result
(** [files ~data_dir] is the list of files in the data dir. If [atify]
    is [true] the files are relative to [data_dir] and prefixed with [@]. *)

(** {1:sections Section}

    Sections are dedicated sub-directories of the data directtory. *)

type section = Boot | Images | Plans | Telvm (** *)
(** The type for data directory sections, i.e. subdirectories. *)

val section_to_string : section -> string
(** [section_to_string s] is a segment name for the section. *)

val section_dir : data_dir:Fpath.t -> rel:bool -> section -> Fpath.t
(** [section_dir ~data_dir section] is the directory to [section] for
    [data_dir], if [rel] is [true] the directory is relative to [data_dir]
    and prefixed with [@]. *)

val section_files :
  data_dir:Fpath.t -> section -> rel:bool -> (Fpath.t list, string) result
(** [section_files] is like {!files} but only lists the files in the
    given section. *)
