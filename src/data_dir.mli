(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data directory management. *)

open B0_std

(** {1:path \@path handling} *)

val is_at_path : Filepath.t -> bool
(** [is_at_path p] is [true] iff [p] starts with [@]. *)

val atify_path : Filepath.t -> Filepath.t
(** [atify_path p] prefixes [p] with [@]. [p] is assumed to be relative. *)

val maybe_atify_path : data_dir:Filepath.t -> Filepath.t -> Filepath.t
(** [maybe_atify_path ~data_dir p] atifies [p] iff [data_dir]
    is a strict prefix of [p]. *)

val resolve_path : data_dir:Filepath.t -> Filepath.t -> Filepath.t
(** [resolve_path ~data_dir ~default p] is [p] if [p] does not start
    with [@], otherwise replaces [@] by the [data_dir] path. The path
    may not exist. *)

(** {1:file Data directory files} *)

val files : data_dir:Filepath.t -> atify:bool -> (Filepath.t list, string) result
(** [files ~data_dir] is the list of files in the data dir. If [atify]
    is [true] the files are relative to [data_dir] and prefixed with [@]. *)

(** {1:sections Section}

    Sections are dedicated sub-directories of the data directtory. *)

type section = Boot | Images | Plans | Telvm (** *)
(** The type for data directory sections, i.e. subdirectories. *)

val section_to_string : section -> string
(** [section_to_string s] is a segment name for the section. *)

val section_dir : data_dir:Filepath.t -> rel:bool -> section -> Filepath.t
(** [section_dir ~data_dir section] is the directory to [section] for
    [data_dir], if [rel] is [true] the directory is relative to [data_dir]
    and prefixed with [@]. *)

val section_files :
  data_dir:Filepath.t -> section -> rel:bool -> (Filepath.t list, string) result
(** [section_files] is like {!files} but only lists the files in the
    given section. *)
