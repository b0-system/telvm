(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Disk images. *)

open B0_std

(** {1:byte_size Bytes sizes} *)

type byte_size = int
(** The type for file system sizes. *)

val kb_to_byte : int -> int
(** [kb_to_byte n] is [n] KB in bytes. *)

val mb_to_byte : int -> int
(** [mb_to_byte n] is [n] MB in bytes. *)

val gb_to_byte : int -> int
(** [gb_to_byte n] is [n] GB in bytes. *)

val byte_size_gb_multiple : int -> int
(** [byte_size_gb_multiple n] rounds [n] bytes up to the neareast
    full GB. *)

(** {1:images Disk images} *)

type partition_scheme = Mbr | Gpt (** *)
(** The type for partition schemes. *)

type file_system = Exfat | Fat32 (** *)
(** The type for file systems.  *)

val make :
  partition_scheme:partition_scheme -> file_system:file_system ->
  byte_size:int -> name:string option -> force:bool -> make_path:bool ->
  Fpath.t -> (unit, string) result
(** [name] maybe truncated depending on [format]. *)

val with_mount' :
  image:Fpath.t -> (mount_root:Fpath.t -> 'a) -> ('a, string) result
(** [with_mount' ~image f] tries to mount [image] in a temporary
    directory [mount_root] and hands it out to [f] for processing.
    The image is unmounted when [f] returns. *)

val with_mount :
  image:Fpath.t -> (mount_root:Fpath.t -> ('a, string) result) ->
  ('a, string) result
(** {!with_mount'} is like {!with_mount} but it {!Result.join}s
    the result of [f]. *)

(** {1:paths Path tools} *)

val cut_file : Fpath.t -> ((Fpath.t * Fpath.t) option, string) result
(** [cut_file p] is [Some (file, q)] with [file] an existing regular
    file on the file system and [q] the remaining path in [p] as an
    absolute path. If [p] is exactly an existing file then the result
    is [None]. In this case [p/] is [Some (p, Fpath.root p].
    The syntactic directoryness of [p] is kept in [q]. *)

val path_in_mount_to_image_path :
  image:Fpath.t -> mount_root:Fpath.t -> Fpath.t -> Fpath.t
(** [path_in_mount_to_image_path ~image ~mount_root p] drops [mount_root]
    from [p] and prepends [image] to id.

    @raise Invalid_argument if [mount_root] is not a prefix of [p]. *)

val get_existing_path_in_mount :
  image:Fpath.t -> mount_root:Fpath.t -> Fpath.t -> (Fpath.t, string) result
(** [path_in_mount_root ~image ~mount_root p] checks that the absolute
    path [p] (usually obtained via {!cut_file}) exists in
    [mount_root], that it does not goes above the mount point
    [mount_root] and returns its path prepended by [mount_root].  This
    is not syntactic it's based on [realpath].

    [image] is the path to the image, it is used for reporting errors
    as if they originate from a sub path of this file. *)

val get_syntactic_path_in_mount :
  image:Fpath.t -> mount_root:Fpath.t -> Fpath.t -> (Fpath.t, string) result
(** [ensure_path_in_mount] is like {!get_existing_path_in_mount}
    execept the path may not exist. *)
