(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line commonalities *)

open B0_std
open Cmdliner

(** {1:convs Converters} *)

val arch_conv : Os.Arch.t Arg.Conv.t
val data_dir_path_conv : Fpath.t Arg.Conv.t
val drive_device_and_data_dir_path_conv :
  (Qemu.drive_device option * Fpath.t) Arg.Conv.t
val image_path_conv : Fpath.t Arg.Conv.t

(** {1:cli Cli fragments} *)

val config : Tool_config.t Term.t
val resolved : bool Term.t
val force : bool Term.t
val dry_run : bool Term.t
val with_shutdown : bool Term.t

val disk_partition_scheme : Disk.Partition.scheme option Term.t
val image_file_system : Disk_image.file_system Term.t
val byte_size : default:string -> int Term.t
val accel : string option Term.t
val mem_size : string option Term.t

(** {2:arch Architecture} *)

val arch :
  ?opts:string list -> ?doc:string -> absent:string -> unit ->
  Os.Arch.t option Term.t

val arch_default_match_host :
  ?opts:string list -> ?doc:string -> unit -> Os.Arch.t Term.t

(** {1:cmds Generic commands} *)

val list_cmd : ?doc:string -> Data_dir.section -> Cmd.Exit.code Cmd.t

(** {1:errors Generic errors} *)

val err_file_exists_use_force : B0_std.Fpath.t -> string
val err_not_dir : Fpath.t -> string
val err_not_file_or_dir : Fpath.t -> string
val err_no_such_file_or_dir : Fpath.t -> string
