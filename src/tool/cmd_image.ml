(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Path resolving *)

let create ~conf ~force ~byte_size ~partition_scheme ~file_system image =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Telvm_conf.data_dir conf in
  let* () =
    let image = Data_dir.resolve_path ~data_dir image in
    let name = None and make_path = true in
    Image.make
      ~file_system ~partition_scheme ~byte_size ~name ~force ~make_path image
  in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let envs = Telvm_cli.conf_envs
let create =
  let doc = "Create an empty disk image" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) creates an empty disk image." ]
  in
  Cmd.make (Cmd.info "create" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf
  and+ byte_size = Telvm_cli.byte_size ~default:"1G"
  and+ partition_scheme = Telvm_cli.image_partition_scheme
  and+ file_system = Telvm_cli.image_file_system and+ force = Telvm_cli.force
  and+ image =
    let doc = "$(docv) is the path to disk image to create." in
    let docv = "[@]IMAGE" in
    Arg.(required & pos 0 (some Telvm_cli.data_dir_path_conv) None &
         info [] ~doc ~docv)
 in
 create ~conf ~force ~byte_size ~partition_scheme ~file_system image

let list = Telvm_cli.list_cmd Images

let cmd =
  let doc = "Operate on disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) operates on disk images.";
    `P "For disk images that are meant to be bootable, specifying the
        architecture somewhere as $(b,arm64) or $(b,x86_64)
        in the image filename helps $(tool) $(b,run) make better automated \
        decisions. Image that do not sport such labels are expected to match \
        the architecture of the host, unless the $(b,--guest-arch) option is \
        used.";]
  in
  Cmd.group (Cmd.info "image" ~doc ~man ~envs) @@
  [create; list]
