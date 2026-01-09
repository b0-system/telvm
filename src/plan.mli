(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(** Disk image build plans. *)

type create_copy_dir = { src : string; dst : string }

type create_user =
  { groups : string list;
    password : string;
    username : string; }

type create_winvos =
  { add_wow64_packages : bool;
    append_to_path : string list;
    boot_execs : string list;
    copy_boot_files : bool;
    packages : string list;
    virtio_drivers : string list; }

type create =
  { copy_dirs : create_copy_dir list;
    users : create_user list;
    winvos : create_winvos; }

type t =
  { arch : Os.Arch.t option;
    basename : string;
    create : create;
    version : int; }

val of_ini : ?file:Fpath.t -> string -> (t, string) result

(*
val pp : t Fmt.t
*)

(* Create image *)

val to_winvos_create_image_cmd :
  data_dir:Fpath.t -> arch:Os.Arch.t ->
  with_shutdown:bool -> t -> (string, string) result
