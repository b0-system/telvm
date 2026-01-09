(*---------------------------------------------------------------------------
   Copyright (c) 2026 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Disks and partitions. *)

open B0_std

(** {1:disks Disks} *)

type t
(** The type for disks *)

val make : ?sector_size:int -> Unix.file_descr -> (t, string) result
(** [make fd] reads a disk from [fd]. The disk is assumed to start at
    the current file descriptor position and to have sectors of byte
    size [sector_size]. *)

val sector_size : t -> int
(** [sector_size d] is the sector size of [d]. *)

val first_byte : t -> int
(** [first_byte d] is the first byte of [d]. This is the [fd]
    position when {!make} was called. *)

val fd : t -> Unix.file_descr
(** [fd] is the file desciptor of [fd]. *)

(** {1:lba Logical block addressing} *)

type lba = int
(** The type for logical block addreses. *)

val lba_first_byte : t -> lba:lba -> int
(** [lba_first_byte lba] is the position of the first byte of
    [lba]. *)

val seek_lba_pos : t -> lba:lba -> pos:int -> unit
(** [seek_lba_pos disk ~lba ~pos] seeks [disk] to byte position [pos]
    of [lba]. *)

(** {1:fs_parts File systems and partitions} *)

(** File systems. *)
module File_system :sig

  (** {1:fs File systems} *)

  type disk := t

  type type' = Exfat | Ext | Fat16 | Fat32 | Iso_9660 | Iso_13490 | Udf (** *)
  (** The type for types of file systems. *)

  val pp_type : type' Fmt.t
  (** [pp_type] formats file system types for inspection. *)
end

(** Disk partitions. *)
module Partition : sig

  (** {1:partition_schemes Partition schemes} *)

  type disk := t

  type scheme =
  | Mbr (** {{:https://en.wikipedia.org/wiki/Master_boot_record}Master boot
            record} *)
  | Gpt (** {{:https://en.wikipedia.org/wiki/GUID_Partition_Table}GUID
            Partition Table} *)
  | Unpartitioned of File_system.type'
    (** No partition scheme, just a file system *)
  (** The type for disk partition schemes. *)

  val pp_scheme : scheme Fmt.t
  (** [pp_scheme] prints partition schemes for inspection. *)

  val read_scheme : disk -> (scheme option, string) result
  (** [read_partition_scheme d] is the parition scheme of [d]. If [None]
      is returned no known partition scheme or file system could be found. *)

  (** {1:partition_type_guids Partition types} *)

  type type' =
  | Efi_system
  | Legacy_mbr
  | Microsoft_bdp
  | Unused
  | Unknown of Uuidm.t
  (** The type for partition types. *)

  val type_of_type_guid : Uuidm.t -> type'
  (** [type_of_type_guid guid] maps know partition type GUIDs to
      {!type'} values. *)

  val type_guid_of_type : type' -> Uuidm.t
  (** [type_guid_of_type t] is the GUID of [t]. *)

  val pp_type : type' Fmt.t
  (** [pp_type] formats types for inspection. *)

  (** {1:partitions Partitions} *)

  type t
  (** The type for disk partitions entries. *)

  val type_guid : t -> Uuidm.t
  (** [type_guid p] is the partition type GUID. *)

  val guid : t -> Uuidm.t
  (** [guid p] is the Unique partition GUID. *)

  val first_lba : t -> lba
  (** [first_lba p] is the first LBA of the partition. *)

  val last_lba : t -> lba
  (** [last_lba p] is the last LBA of the partition. *)

  val attributes : t -> int64
  (** [attributes p] are the attributes of the partition. *)

  val name : t -> string
  (** [name p] is the UTF-8 encoded partition name. *)

  val pp : ?disk:disk -> unit -> t Fmt.t
  (** [pp] formats partitions for inspection. *)

  val read_entries : disk -> (scheme * t list, string) result
  (** [read_entries disk] are the partition entries of [disk], unused
      entries are not reported. Partitions entries are represented
      with the GPT model. This means that:

      {ul
      {- For [MBR] partitions, they are translated to this format.
         If possible a suitable {!type_guid} is found. Otherwise
         {!Uuidm.max} is used.}
      {- For [Unpartitioned] partitions, both the {!type_guid} and {!guid} are
         set to {!Uuidm.max} and first and last LBA simply extent the
         file size.}}

      This errors if there is no partition scheme or volume can be found
      on [disk]. *)

  val read_file_system_type :
    disk -> t -> (File_system.type' option, string) result
  (** [read_file_system_type disk] moves to the lba of [partition] and tries
      to detect a file system. *)
end
