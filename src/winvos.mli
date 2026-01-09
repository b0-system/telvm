(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** WinVOS munging *)

open B0_std

val archs : Os.Arch.t list
(** [archs] is the list of architectures supported by WinVOS. *)

(** {1:upstream_data Upstream data} *)

val virtio_win_iso : Data_file.t
(** [virtio-win.iso] file. *)

val iso : arch:Os.Arch.t -> (Data_file.t, string) result
(** [winvos-$(arch).iso] files. *)

val powershell_zip : arch:Os.Arch.t -> (Data_file.t, string) result
(** [powershell-$(arch).zip] files. *)

val get_or_download_virtio_win_iso :
  data_dir:Fpath.t -> (Fpath.t, string) result

val get_or_download_iso :
  data_dir:Fpath.t -> arch:Os.Arch.t -> (Fpath.t, string) result

val get_or_download_powershell_zip :
  data_dir:Fpath.t -> arch:Os.Arch.t -> (Fpath.t, string) result

(** {1:version Version} *)

val upstream_version : arch:Os.Arch.t -> (string, string) result
(** [upstream_version ~arch] looks up the latest version published
    upstream for [arch]. *)

val installed_version :
  data_dir:Fpath.t -> arch:Os.Arch.t -> (string option, string) result
(** [installed_version ~data_dir ~arch] is the installed version for [arch]
    (if installed). *)

(*
val installed_powershell_version :
  data_dir:Fpath.t -> arch:Os.Arch.t -> (string option, string) result
*)

(** {1:packages Packages} *)

val cab_extract_file :
  force:bool -> make_path:bool -> cab:Fpath.t -> file:string ->
  dst_dir:Fpath.t -> (unit, string) result

(** Packages ([.cab] files) *)
module Pkg : sig
  type t
  (** The type for WinVOS cab packages *)

  val name : t -> string
  (** [name pkg] is the basename of the package. *)

  val files : t -> Fpath.t list
  (** [files pkg] are the files of package [pkg] relative to
      the validation OS image root. *)

  val list :
    data_dir:Fpath.t -> arch:Os.Arch.t ->
    names:string list-> add_matching_wow64:bool ->
    (t list, string) result
  (** [list pkg] are the available packages for architecture [arch].
      If [names] is non-empty only lists those and errors or not found
      packages. If [add_matching_wow64] is [true] and [names] non-empty,
      matching WOW64 packages are added to the result. *)

  val contents :
    data_dir:Fpath.t -> arch:Os.Arch.t -> names:string list ->
    add_matching_wow64:bool -> (unit, string) result

  val find : string -> t list -> t option
  (** [find name pkgs] finds the package named [pkg]. *)
end

(** {1:imaging Imaging} *)

val copy_winvos_vhdx :
  force:bool -> make_path:bool -> winvos_mount:B0_std.Fpath.t ->
  dst:B0__fpath.t -> (unit, string) result
(** [copy_winvos_vhdx] copies the Validation OS vhdx from a
    WinVOS mount to [dst]. *)


(** WinVOS built-in plans *)
module Plan : sig
  val imager_build_script_src :
    bootstrap_arch:Os.Arch.t -> imager_arch:Os.Arch.t -> string option
  (** [imager_build_script_src ~bootstrap_arch ~imager_arch] is the
      [.cmd] build script source for running on [bootstrap_arch] that
      generates the imager image of architecture [imager_arch] *)

  val base_src : string
  (** [base_src] is the source of the base WinVOS plan of [telvm]. *)
end

(** Imager image. *)
module Imager : sig

  val get_bootstrap_arch : unit -> Os.Arch.t
  (** [get_boostrap_arch ()] returns the architecture of the
      OS run to perform the boostrap. *)

  (** {1:build_script Imager build script} *)

  val get_build_script_src :
    data_dir:Fpath.t -> bootstrap_arch:Os.Arch.t -> imager_arch:Os.Arch.t ->
    Fpath.t option -> (string, string) result
  (** [get_script_src] is like {!Plan.imager_build_script_src} on [None]
      otherwise looks up and reads the given file (possibly resolved in
      the [data_dir]) *)

  (** {1:imager Imager} *)

  val get_filepath :
    data_dir:Fpath.t -> imager_arch:Os.Arch.t -> Fpath.t option -> Fpath.t
  (** [get_filepath ~data_dir ~imager_arch img] resolves the file path
      [img] to file system path (not necessarily existing). *)

  val make :
    data_dir:Fpath.t -> accel:string option -> mem_size:string option ->
    bootstrap_arch:Os.Arch.t -> build_script_src:string ->
    imager_arch:Os.Arch.t -> force:bool -> make_path:bool ->
    dst:Fpath.t option -> (unit, string) result
  (** [make_imager] creates an imager image of architecture [imager_arch]
      in [arch] by boostrapping with the WinVOS image of [bootstrap_arch]
      using the build script source [build_script_src]. *)

  val get :
    data_dir:Fpath.t -> imager_arch:Os.Arch.t option ->
    Fpath.t option -> (Fpath.t * Os.Arch.t, string) result
  (** [get ~data_dir ~bootstrap_arch file] gets an imager from [file]
      if specified. If unspecified looks for an imager in the [data_dir]
      and if not found bootstraps via {!make} using default parameters. *)
end
