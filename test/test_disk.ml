(*---------------------------------------------------------------------------
   Copyright (c) 2026 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_testing

let test_fs_magic =
  Test.test "Disk and Image: recognize what we create" @@ fun () ->
  let test_disk partition_scheme fs =
    Test.error_to_fail @@ Result.join @@
    Os.File.with_tmp ~name:"disk-%s.img" @@ fun file ->
    let byte_size = Byte_size.of_mb 50 in
    let* () =
      Disk_image.make ~partition_scheme ~file_system:fs ~byte_size
        ~name:None ~force:true ~make_path:false file
    in
    Result.join @@ Os.File.read_with_fd file @@ fun fd ->
    let* disk = Disk.make fd in
    let* fnd_scheme, partition = Disk.Partition.read_entries disk in
    let first = List.hd partition in
    let* fs_type = Disk.Partition.read_file_system_type disk first in
    if fnd_scheme <> partition_scheme then
      Fmt.error "Partition scheme %a not recognized found: %a"
        Disk.Partition.pp_scheme partition_scheme
        Disk.Partition.pp_scheme fnd_scheme
    else
    match fs_type with
    | Some Fat32 when fs = Fat32 -> Ok (Test.pass ())
    | Some Exfat when fs = Exfat -> Ok (Test.pass ())
    | _ ->
        Fmt.error
          "File system %a not recognized" Disk_image.pp_file_system fs
  in
  test_disk Gpt Exfat;
  test_disk Mbr Exfat;
  test_disk (Unpartitioned Exfat) Exfat;
  test_disk Gpt Fat32;
  test_disk Mbr Fat32;
  test_disk (Unpartitioned Fat32) Fat32;
  ()


let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
