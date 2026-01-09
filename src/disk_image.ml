(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let handle_fs ~force ~make_path file =
  let* exists = Os.File.exists file in
  if exists && not force then Fpath.error file "File exists" else
  let parent_dir = Fpath.parent file in
  (* handle [make_path] *)
  let* dir_exists = Os.Dir.exists parent_dir in
  if dir_exists then Ok () else
  if make_path
  then Result.map ignore (Os.Dir.create ~make_path:true parent_dir)
  else Fpath.error parent_dir "No such directory"

(* Disk images *)

type file_system = Exfat | Fat32
let pp_file_system ppf = function
| Exfat -> Fmt.string ppf "exFAT"
| Fat32 -> Fmt.string ppf "Fat32"

let volname ~name ~file_system file =
  let name = match name with
  | Some name -> name | None -> Fpath.basename ~drop_exts:true file
  in
  match file_system with
  | Exfat | Fat32 ->
      (* Note: hdiutil fails with with an obscure unrelated error
         if this is not enforced *)
      String.Ascii.uppercase (String.take_first 11 name)

let make_blank_image ~force ~make_path ~byte_size file =
  Result.join @@ Os.File.write_with_fd ~force ~make_path file @@ fun fd ->
  try Ok (Os.Fd.ftruncate fd byte_size) with
  | Unix.Unix_error (e, _, _)  -> Fpath.error file "%s" (Unix.error_message e)

let err_mount_failure ~image e =
  Fmt.str "@[<v>Failed to mount image %a:@,%s@]" Fpath.pp image e

module Macos = struct
  let hdiutil_create ~byte_size ~fs ~layout ~volname ~image =
    let size_kb = Byte_size.to_kb byte_size in
    let size_kb = Fmt.str "%dk" size_kb in
    Cmd.(tool "hdiutil" % "create" % "-size" % size_kb %
         "-layout" % layout % "-fs" % fs % "-volname" % volname %
         "-type" % "UDTO" %% path image)

  let hdiutil_detach ~dev = Cmd.(tool "hdiutil" % "detach" % "-force" % dev)
  let hdiutil_attach ~mount_root ~image =
    Cmd.(tool "hdiutil" % "attach" % "-mountpoint" %% path mount_root %%
         path image)

  let partition_scheme_to_layout_arg = function
  | Disk.Partition.Mbr -> "MBRSPUD" | Disk.Partition.Gpt -> "GPTSPUD"
  | Disk.Partition.Unpartitioned _ -> "NONE"

  let file_system_to_fs_arg = function
  | Exfat -> "ExFat" | Fat32 -> "MS-DOS FAT32"

  let make
      ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path file
    =
    let* () = handle_fs ~force ~make_path file in
    let layout = partition_scheme_to_layout_arg partition_scheme in
    let fs = file_system_to_fs_arg file_system in
    let volname = volname ~file_system ~name file in
    let file_cdr =
      (* hdiutil unhelpfully adds .cdr on -type UDTO if the file doesn't
         end so, we rename afterwards *)
      Fpath.(file -+ ".cdr")
    in
    let hdiutil =
      hdiutil_create ~byte_size ~fs ~layout ~volname ~image:file_cdr
    in
    let* _stdout = Os.Cmd.run_out ~stderr:`Out ~trim:false hdiutil in
    Os.Path.rename ~force:true ~make_path:false file_cdr ~dst:file

  let with_mount ~image f =
    Result.map_error (err_mount_failure ~image) @@
    let* () = Os.File.must_exist image in
    Result.join @@ Os.Dir.with_tmp @@ fun mount_root ->
    let* mount_root =
      (* We make sure to have the realpath here otherwise subsequent
         operations that realpath objects in [f] may end up not being
         prefixed by [mount_root]. *)
      Os.Path.realpath mount_root
    in
    let* info =
      Os.Cmd.run_out ~stderr:`Out ~trim:true (hdiutil_attach ~mount_root ~image)
    in
    let* dev = match String.split_first ~sep:" " info with
    | Some (dev, _) -> Ok dev
    | None -> Fmt.error "Could not parse device from %S" info
    in
    let unmount () =
      Log.if_error ~use:() @@
      Result.map ignore @@
      Os.Cmd.run_out ~stderr:`Out ~trim:false (hdiutil_detach ~dev)
    in
    (* Be careful with user Ctrl-c *)
    Os.Exit.on_sigint ~hook:unmount @@ fun () ->
    Ok (Fun.protect ~finally:unmount @@ fun () -> f ~mount_root)
end

module Linux = struct

  let first_partition_first_byte image =
    Result.join @@ Os.File.read_with_fd image @@ fun fd ->
    let* disk = Disk.make fd in
    let* scheme, partitions = Disk.Partition.read_entries disk in
    match partitions with
    | [] -> Error "Could not find a partition to mount"
    | p :: _ -> Ok (Disk.lba_first_byte disk ~lba:(Disk.Partition.first_lba p))

  let with_mount ~image f =
    Result.map_error (err_mount_failure ~image) @@
    let* () = Os.File.must_exist image in
    let* first_byte = first_partition_first_byte image in
    Result.join @@ Os.Dir.with_tmp @@ fun mount_root ->
    let* mount_root =
      (* We make sure to have the realpath here otherwise subsequent
         operations that realpath objects in [f] may end up not being
         prefixed by [mount_root]. *)
      Os.Path.realpath mount_root
    in
    let loop = Fmt.str "loop,offset=%d" first_byte in
    let mount =
      Cmd.(tool "mount" % "-o" % loop %% path image %% path mount_root)
    in
    let unmount = Cmd.(tool "umount" %% path mount_root) in
    let* _noise = Os.Cmd.run_out ~stderr:`Out ~trim:false mount in
    let unmount () =
      Log.if_error ~use:() @@
      Result.map ignore (Os.Cmd.run_out ~stderr:`Out ~trim:false unmount)
    in
    (* Be careful with user Ctrl-c *)
    Os.Exit.on_sigint ~hook:unmount @@ fun () ->
    Ok (Fun.protect ~finally:unmount @@ fun () -> f ~mount_root)

  let parted_write_partition_scheme ~partition_scheme ~file_system file =
    (* XXX now that we read GPT and MPR we could write that ourselves
       to trim on the needed tools *)
    let label = match partition_scheme with
    | Disk.Partition.Mbr -> "msdos"
    | Disk.Partition.Gpt -> "gpt"
    | Disk.Partition.Unpartitioned _ -> assert false
    in
    let fs = match file_system with Exfat -> "ntfs" | Fat32 -> "fat32" in
    let parted =
      Cmd.(tool "parted" % "-s" %% path file % "mklabel" % label %
           "mkpart" % "primary" % fs % "0%" % "100%")
    in
    Os.Cmd.run parted

  let mkfs_exfat ~volume_label dev =
    let mkfs_exfat =
      Cmd.(tool "mkfs.exfat" % "--quiet" % "--volume-label" % volume_label %%
           path dev)
    in
    Os.Cmd.run mkfs_exfat

  let mkfs_fat32 ~volume_label dev =
    let mkfs_fat =
      Cmd.(tool "mkfs.fat" % "-F" % "32" % "-n" % volume_label %% path dev)
    in
    let* _stdout (* no quiet mode *) = Os.Cmd.run_out ~trim:false mkfs_fat in
    Ok ()

  let mkfs ~file_system ~name dev =
    let volume_label = volname ~file_system ~name dev in
    match file_system with
    | Exfat -> mkfs_exfat ~volume_label dev
    | Fat32 -> mkfs_fat32 ~volume_label dev

  let make
      ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path file
    =
    let* () = make_blank_image ~force ~make_path ~byte_size file in
    match partition_scheme with
    | Disk.Partition.Unpartitioned _ ->
        mkfs ~file_system ~name file
    | Mbr | Gpt ->
        let* () =
          parted_write_partition_scheme ~partition_scheme ~file_system file
        in
        let* first_byte = first_partition_first_byte file in
        let losetup =
          Cmd.(tool "losetup" % "-f" % "--show" % "-o" %%
               int first_byte %% path file)
        in
        let* dev = Os.Cmd.run_out ~trim:true losetup in
        let finally () =
          ignore (Os.Cmd.run Cmd.(tool "losetup" % "-d" % dev))
        in
        Fun.protect ~finally @@ fun () ->
        let* dev = Fpath.of_string dev in
        mkfs ~file_system ~name dev
end

let make
    ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path file
  =
  match Os.name () with
  | Os.Name.Darwin _ ->
      Macos.make
        ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path file
  | Os.Name.Linux _ ->
      Linux.make
        ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path file
  | os -> Fmt.error "%a machine: Image.make is TODO" Os.Name.pp os

let with_mount' ~image f = match Os.name () with
| Os.Name.Darwin _ -> Macos.with_mount ~image f
| Os.Name.Linux _ -> Linux.with_mount ~image f
| os -> Fmt.error "%a machine: Image.with_mount is TODO" Os.Name.pp os

let with_mount ~image f = Result.join (with_mount' ~image f)

(* Path tools *)

let cut_file p =
  try
    let exists p = Os.File.exists p |> Result.error_to_failure in
    if exists p then Ok None else
    let rec loop suffix p =
      if Fpath.is_root p || Fpath.is_current_dir p then Ok None else
      if exists p then
        let suffix = match suffix with
        | [] | [""] -> [""; ""]
        | suffix -> "" :: suffix
        in
        Ok (Some (p, Fpath.of_segments suffix))
      else
      let up, seg = Fpath.cut_last_segment p in
      loop (seg :: suffix) up
    in
    loop [] p
  with
  | Failure e -> Error e

let path_in_mount_to_image_path ~image ~mount_root p =
  match Fpath.drop_strict_prefix ~prefix:mount_root p with
  | None -> assert false | Some p -> Fpath.(image // p)

let rec get_existing_path_in_mount ~image ~mount_root path =
  let* real_mount_root = Os.Path.realpath mount_root in
  let rel = Fpath.drop_root_sep path in
  let path_in_real_mount = Fpath.(real_mount_root // rel) in
  let* real =
    try
      Fpath.of_string (Unix.realpath (Fpath.to_string path_in_real_mount))
    with
    | Unix.Unix_error (EINTR, _, _) ->
        get_existing_path_in_mount ~image ~mount_root rel
    | Unix.Unix_error ((ENOENT|ENOTDIR), _, _) ->
        Fmt.error "%a: No such path" Fpath.pp Fpath.(image // rel)
    | Unix.Unix_error (e, _, _) ->
        Fmt.error "%a: %s" Fpath.pp Fpath.(image // rel) (Unix.error_message e)
  in
  if Fpath.strictly_starts_with ~prefix:real_mount_root real
  then Ok Fpath.(mount_root // rel) else
  if Fpath.equal real_mount_root real
  then Ok mount_root
  else Fmt.error "Path %a points outside the disk image mount" Fpath.pp rel


let get_syntactic_path_in_mount ~image ~mount_root path =
  (* XXX perhaps we should rather make sure with_mount returns
     a realpath *)
  let rel = Fpath.drop_root_sep path in
  let* real_mount_root = Os.Path.realpath mount_root in
  let path = Fpath.try_drop_relative_dirs Fpath.(real_mount_root // rel) in
  if Fpath.strictly_starts_with ~prefix:real_mount_root path
  then Ok Fpath.(mount_root // rel)
  else if Fpath.equal real_mount_root path
  then Ok mount_root
  else Fmt.error "Path %a points outside the disk image mount" Fpath.pp rel
