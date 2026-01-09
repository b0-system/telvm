(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data files stored in the data directory. *)

open B0_std

(** {1:data_files Data files} *)

type src = Url of Net.Url.t (** *)
(** The type for data file sources. *)

type t
(** The type for data files. *)

val make : ?digest:string -> Data_dir.section -> filename:string -> src:src -> t
(** [make section ~file ~src] is a data file named [file] sourced from
    [src] in stored in the data directory section [Data_dir.section]. *)

val filename : t -> string
(** [filename f] is the filename of [f]. *)

val src : t -> src
(** [src f] is the source of [f]. *)

val digest : t -> string option
(** [digest f] is the digest of [f] (if any). *)

val path : data_dir:Fpath.t -> t -> Fpath.t
(** [path ~data_dir] is the complete path to the file in the data directory
    (may not exist). *)

(** {1:operations Operations} *)

val exists : data_dir:Fpath.t -> t -> (bool, string) result
(** [exists ~data_dir f] tests [f] for existence in [data_dir]. *)

val find : data_dir:Fpath.t -> t -> (Fpath.t option, string) result
(** [find ~data_dir f] finds [f] in [data_dir] and returns [Some p]
    with [p = path ~data_dir] if it exists. *)

val get : data_dir:Fpath.t -> t -> (Fpath.t, string) result
(** [get ~data_dir f] gets the file to [f] in [data_dir] and
    errors with an error message if it doesn't exist. *)

val download :
  httpc:B0_http.Http_client.t -> data_dir:Fpath.t -> t ->
  (Fpath.t, string) result
(** [download ~httpc ~data_dir f] downloads [f] to [data_dir] using
    [httpc]. *)

val get_or_download :
  ?available_log:Log.level -> ?httpc:B0_http.Http_client.t ->
  data_dir:Fpath.t -> t -> (Fpath.t, string) result
(** [ensure] is like {!download} but if the file exists skips the download
    and logs the availity with level [available_log] (defaults to
    [Log.Info]) *)
