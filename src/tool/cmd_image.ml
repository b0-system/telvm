(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let create ~config ~force ~byte_size ~partition_scheme ~file_system image =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* () =
    let image = Data_dir.resolve_path ~data_dir image in
    let name = None and make_path = true in
    let partition_scheme = match partition_scheme with
    | None ->
        let file_system = match file_system with
        | Disk_image.Fat32 -> Disk.File_system.Fat32
        | Disk_image.Exfat -> Disk.File_system.Exfat
        in
        Disk.Partition.Unpartitioned file_system
    | Some scheme -> scheme
    in
    Disk_image.make
      ~file_system ~partition_scheme ~byte_size ~name ~force ~make_path image
  in
  Ok 0

let pp_partition disk ppf partition =
  let pp_fs_type =
    Fmt.option ~none:(Fmt.any "<unknown>") Disk.File_system.pp_type
  in
  let fs_type =
    Result.error_to_failure @@
    Disk.Partition.read_file_system_type disk partition
  in
  Fmt.pf ppf "@[<v>%a@,%a@]"
    (Disk.Partition.pp ~disk ()) partition
    (Fmt.field "file-system" Fun.id pp_fs_type) fs_type

let info ~config ~images =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let pp_image ppf image =
    let image = Data_dir.resolve_path ~data_dir image in
    Result.error_to_failure @@ Result.join @@
    Os.File.read_with_fd image @@ fun fd ->
    let* disk = Disk.make fd in
    let* scheme, partitions = Disk.Partition.read_entries disk in
    Fmt.pf ppf "@[<v>%a@,%a@,%a@]"
      (Fmt.field "disk-image" Fun.id Fpath.pp) image
      (Fmt.field "partition-scheme" Fun.id Disk.Partition.pp_scheme) scheme
      (Fmt.field "partitions" Fun.id (Fmt.list (pp_partition disk)))
      partitions;
    Ok ()
  in
  try
    Fmt.pr "@[<v>%a@]@." Fmt.(list ~sep:(any "@,@,") pp_image) images;
    Ok 0
  with
  | Failure e -> Error e


(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let create =
  let doc = "Create an empty disk image" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) creates an empty disk image." ]
  in
  Cmd.make (Cmd.info "create" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ byte_size = Tool_cli.byte_size ~default:"1G"
  and+ partition_scheme = Tool_cli.disk_partition_scheme
  and+ file_system = Tool_cli.image_file_system and+ force = Tool_cli.force
  and+ image =
    let doc = "$(docv) is the path to disk image to create." in
    let docv = "[@]IMAGE" in
    Arg.(required & pos 0 (some Tool_cli.data_dir_path_conv) None &
         info [] ~doc ~docv)
 in
 create ~config ~force ~byte_size ~partition_scheme ~file_system image

let info =
  let doc = "Output information about disk images" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs information about disk images. Partitions are shown \
        as found in the GPT, on MBR disks an ad-hoc translation is performed.";
  ]
  in
  Cmd.make (Cmd.info "info" ~doc ~man) @@
  let+ config = Tool_cli.config
  and+ images =
    let doc = "$(docv) is the path to disk image to consider. Repeatable" in
    let docv = "[@]IMAGE" in
    Arg.(value & pos_all Tool_cli.data_dir_path_conv [] &
         info [] ~doc ~docv)
 in
 info ~config ~images


let list = Tool_cli.list_cmd Images

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
  Cmd.group (Cmd.info "image" ~doc ~man) @@
  [create; info; list; ]
