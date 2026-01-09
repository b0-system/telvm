(*---------------------------------------------------------------------------
   Copyright (c) 2026 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let option_try_value o f = match o with
| None -> f () | Some _ as v -> v

(* Errors *)

let uerror msg e = Fmt.str "%s: %s" msg (Unix.error_message e)
let err_eod = "Unexpected end of disk"
let err_byte_pos e = uerror "Could not determine disk position" e
let err_length e = uerror "Could not determine disk length" e
let err_part_scheme e = Fmt.str "Could not determine partition scheme: %s" e
let err_part_scheme_not_found =
  "No file system or MBR or GPT partition scheme found"
let err_parts e = Fmt.str "Could not determine partitions: %s" e
let err_gpt_partitions e = Fmt.str "Could not read GPT partitions: %s" e
let err_mbr_partitions e = Fmt.str "Could not read MBR partitions: %s" e
let err_rev_number n = Fmt.str "Unknown GPT header revision number: 0x%lx" n
let err_fs_type e = Fmt.str "Could not detect file system: %s" e

(* Disks *)

type t =
  { fd : Unix.file_descr;
    sector_size : int;
    first_byte : int;
    buf : Bytes.t; (* of sector_size *)  }

let fd disk = disk.fd
let sector_size disk = disk.sector_size
let first_byte disk = disk.first_byte
let buf disk = disk.buf

let byte_pos fd = Os.Fd.lseek fd 0 SEEK_CUR

let make ?(sector_size = 512) fd =
  let* first_byte = try Ok (byte_pos fd) with
  | Unix.Unix_error (e, _, _) -> Error (err_byte_pos e)
  in
  Ok { fd; sector_size; first_byte; buf = Bytes.make sector_size '\x00' }

let disk_length disk = match Unix.lseek disk.fd 0 SEEK_END with
| len -> Ok (len - disk.first_byte)
| exception Unix.Unix_error (e, _, _) -> Error (err_length e)

(* Logical block addressing *)

type lba = int

let lba_first_byte disk ~lba = disk.first_byte + lba * disk.sector_size

let pp_lba disk ppf lba =
  let lba_byte_pos = lba_first_byte disk ~lba in
  Fmt.pf ppf "%d (byte position %d)" lba lba_byte_pos

(* Seek operations *)

let seek_lba_pos disk ~lba ~pos =
  let byte_pos = lba_first_byte disk ~lba + pos in
  ignore (Os.Fd.lseek disk.fd byte_pos SEEK_SET)

(* Read operations *)

let read disk ~length =
  if Os.Fd.read disk.fd disk.buf ~first:0 ~length = length
  then () else raise End_of_file

let read_uint8 disk = read disk ~length:1; Bytes.get_uint8 disk.buf 0
let read_uint16_le disk = read disk ~length:2; Bytes.get_uint16_le disk.buf 0
let read_uint32_le disk = read disk ~length:4; Bytes.get_int32_le disk.buf 0
let read_uint64_le disk = read disk ~length:8; Bytes.get_int64_le disk.buf 0
let read_bytes disk ~length =
  read disk ~length; Bytes.sub_string disk.buf 0 length

let read_uint32_le_as_int disk =
  let uint = read_uint32_le disk in
  match Int32.unsigned_to_int uint with
  | None -> Fmt.failwith "Cannot represent 0x%lx on OCaml int" uint
  | Some int -> int

let read_uint64_le_as_int disk =
  let uint = read_uint64_le disk in
  match Int64.unsigned_to_int uint with
  | None -> Fmt.failwith "Cannot represent 0x%Lx on OCaml int" uint
  | Some int -> int

(* File systems *)

module File_system = struct
  type type' =
  | Exfat | Ext | Fat16 | Fat32 | Iso_9660 | Iso_13490 | Udf

  let pp_type ppf = function
  | Exfat -> Fmt.string ppf "exFAT"
  | Ext -> Fmt.string ppf "ext"
  | Fat16 -> Fmt.string ppf "FAT16"
  | Fat32 -> Fmt.string ppf "FAT32"
  | Iso_9660 -> Fmt.string ppf "ISO-9960"
  | Iso_13490 -> Fmt.string ppf "ISO-13490"
  | Udf -> Fmt.string ppf "UDF"

  let find_fat disk ~lba =
    seek_lba_pos disk ~lba ~pos:3;
    match read_bytes disk ~length:8 with
    | exception End_of_file -> None
    | "EXFAT   " -> Some Exfat
    | _ ->
        seek_lba_pos disk ~lba ~pos:510;
        match read_uint16_le disk with
        | 0xAA55 ->
            seek_lba_pos disk ~lba ~pos:38;
            begin match read_uint8 disk with
            | 0x28 | 0x29 -> Some Fat16 (* Technically could be Fat16 *)
            | exception End_of_file -> None
            | _ ->
                seek_lba_pos disk ~lba ~pos:66;
                match read_uint8 disk with
                | 0x28 | 0x29 -> Some Fat32
                | exception End_of_file | _ -> None
            end
        | exception End_of_file | _ -> None

  let find_ext disk ~lba =
    seek_lba_pos disk ~lba ~pos:1080;
    match read_uint16_le disk with
    | 0xEF53 -> Some Ext
    | exception End_of_file | _ -> None

  let find_iso_9660_or_udf disk ~lba =
    (* This is a very rough approximation of the volume recognition sequence
       of ECMA-167. We read volume descriptors until we don't recognize
       a volume descriptor id (apparently terminators are not…), along
       the way if we see a CDW02 we stop as Iso_13490, NSR02 or NSR03
       we stop as Udf and otherwise default to Iso_9660. *)
    let rec loop disk ~lba ~block =
      seek_lba_pos disk ~lba ~pos:(32_768 + (block * 2048) + 1);
      match read_bytes disk ~length:5 with
      | "CD001" -> loop disk ~lba ~block:(block + 1)
      | "BEA01" | "TEA01" | "BOOT2" -> loop disk ~lba ~block:(block + 1)
      | "NSR02" | "NSR03" -> Some Udf
      | "CDW02" -> Some Iso_13490
      | exception End_of_file | _ -> if block = 0 then None else Some Iso_9660
    in
    loop disk ~lba ~block:0

  let read_type disk ~lba =
    try
      Result.ok @@
      option_try_value (find_fat disk ~lba) @@ fun () ->
      option_try_value (find_ext disk ~lba) @@ fun () ->
      find_iso_9660_or_udf disk ~lba
    with
    | Failure e -> Error (err_fs_type e)
    | End_of_file -> Error (err_fs_type err_eod)
    | Unix.Unix_error (e, _, _) -> Error (err_fs_type (Unix.error_message e))

end

(* Partitions *)

module Partition = struct
  (* See https://uefi.org/specs/UEFI/2.11/05_GUID_Partition_Table_Format.html *)

  type scheme = Mbr | Gpt | Unpartitioned of File_system.type'

  let pp_scheme ppf = function
  | Mbr -> Fmt.string ppf "MBR"
  | Gpt -> Fmt.string ppf "GPT"
  | Unpartitioned fs ->
      Fmt.pf ppf "Unpartitioned (%a file system)" File_system.pp_type fs


  let find_gpt disk =
    seek_lba_pos disk ~lba:1 ~pos:0;
    if read_bytes disk ~length:8 = "EFI PART" then Some Gpt else None

  let find_mbr disk =
    seek_lba_pos disk ~lba:0 ~pos:510;
    if read_bytes disk ~length:2 <> "\x55\xAA" then None else
    begin
      (* This could be an unpartitioned FAT image *)
      seek_lba_pos disk ~lba:0 ~pos:0;
      let start = read_bytes disk ~length:3 in
      if String.get_uint8 start 0 = 0xEB &&
         String.get_uint8 start 2 = 0x90
      then (* Looks like a FAT disk *) None
      else Some Mbr
    end

  let read_scheme disk =
    try
      Result.ok @@
      option_try_value (find_gpt disk) @@ fun () ->
      option_try_value (find_mbr disk) @@ fun () ->
      let fs = File_system.read_type disk ~lba:0 |> Result.error_to_failure in
      match fs with
      | None -> None
      | Some fs -> (Some (Unpartitioned fs))
    with
    | Failure e -> Error (err_part_scheme e)
    | End_of_file -> Error (err_part_scheme err_eod)
    | Unix.Unix_error (e, _, _) ->
        Error (err_part_scheme (Unix.error_message e))

  type type' =
  | Efi_system
  | Legacy_mbr
  | Microsoft_bdp
  | Unused
  | Unknown of Uuidm.t

  let types =
    let u s = Uuidm.of_string s |> Option.get in
    [ Efi_system, u "C12A7328-F81F-11D2-BA4B-00A0C93EC93B";
      Legacy_mbr, u "024DEE41-33E7-11D3-9D69-0008C781F39F";
      Microsoft_bdp, u "EBD0A0A2-B9E5-4433-87C0-68B6B72699C7";
      Unused, Uuidm.nil ]

  let type_of_type_guid guid =
    let is_guid (_, g) = Uuidm.equal guid g in
    match List.find_opt is_guid types with
    | None -> Unknown guid | Some (t, _) -> t

  let type_guid_of_type = function
  | Unknown guid -> guid
  | t ->
      let is_type (u, _) = t = u in
      match List.find_opt is_type types with
      | None -> assert false | Some (_, guid) -> guid

  let type_mbr_to_type_guid = function
  | 0x00 -> type_guid_of_type Unused
  | 0x07 | 0x0B | 0x0C | 0x0D -> type_guid_of_type Microsoft_bdp
  | 0xEF -> type_guid_of_type Efi_system
  | _ -> Uuidm.max

  let pp_type ppf = function
  | Efi_system -> Fmt.string ppf "EFI System"
  | Legacy_mbr -> Fmt.string ppf "Legacy MBR"
  | Microsoft_bdp -> Fmt.string ppf "Microsoft BDP (FAT, ExFAT, NTFS)"
  | Unused -> Fmt.string ppf "Unused"
  | Unknown uuid -> Uuidm.pp ppf uuid

  type t =
    { type_guid : Uuidm.t;
      guid : Uuidm.t;
      first_lba : int;
      last_lba : int;
      attributes : int64;
      name : string; }

  let type_guid p = p.type_guid
  let guid p = p.guid
  let first_lba p = p.first_lba
  let last_lba p = p.last_lba
  let attributes p = p.attributes
  let name p = p.name

  let decode_uuid s = Uuidm.of_mixed_endian_binary_string s |> Option.get

  let decode_utf_16le_name name =
    let buf = Buffer.create 72 in
    let null = Uchar.of_int 0x00 in
    try
      for i = 0 to 36 do
        let udec = String.get_utf_16le_uchar name (i * 2) in
        let uchar = Uchar.utf_decode_uchar udec in
        if Uchar.equal uchar null then raise Exit else
        Buffer.add_utf_8_uchar buf uchar
      done;
      Buffer.contents buf
    with
    | Exit -> Buffer.contents buf

  let read_gpt_partition_entry disk =
    let type_guid = decode_uuid (read_bytes disk ~length:16) in
    let guid = decode_uuid (read_bytes disk ~length:16) in
    let first_lba = read_uint64_le_as_int disk in
    let last_lba = read_uint64_le_as_int disk in
    let attributes = read_uint64_le disk in
    let name = decode_utf_16le_name (read_bytes disk ~length:72) in
    { type_guid; guid; first_lba; last_lba; attributes; name }

  let read_gpt_partitions disk =
    (* Very primitive for now, no CRC-32 check no lookup in the backup
       partition table. Various integrity constraints are not checked.
       Error report is miserable. *)
    try
      seek_lba_pos disk ~lba:1 ~pos:8;
      let rev = read_uint32_le disk in
      if rev > 0x00010000l then failwith (err_rev_number rev) else
      begin
        seek_lba_pos disk ~lba:1 ~pos:72;
        let part_entry_lba = read_uint64_le_as_int disk in
        let part_entry_count = read_uint32_le_as_int disk in
        let part_entry_size = read_uint32_le_as_int disk in
        let parts = ref [] in
        for i = 0 to part_entry_count - 1 do
          let pos = i * part_entry_size in
          seek_lba_pos disk ~lba:part_entry_lba ~pos;
          let part = read_gpt_partition_entry disk in
          if Uuidm.equal part.type_guid Uuidm.nil
          then ()
          else parts := part :: !parts
        done;
        Ok (List.rev !parts)
      end
    with
    | End_of_file ->
        Error (err_gpt_partitions err_eod)
    | Unix.Unix_error (e, _, _) ->
        Error (err_gpt_partitions (Unix.error_message e))
    | Failure e ->
        Error (err_gpt_partitions e)

  let read_mbr_partition disk =
    let read_chs disk =
      let c = read_uint8 disk in
      let h = read_uint8 disk in
      let s = read_uint8 disk in
      (c,h,s)
    in
    let _boot_indicator = read_uint8 disk in
    let _starting_chs = read_chs disk in
    let part_type = read_uint8 disk in
    let _ending_chs = read_chs disk in
    let first_lba = read_uint32_le_as_int disk in
    let size_in_lba = read_uint32_le_as_int disk in
    let last_lba = first_lba + size_in_lba - 1 in
    let type_guid = type_mbr_to_type_guid part_type in
    let guid = Uuidm.max in
    { type_guid; guid; first_lba; last_lba; attributes = 0x0L; name = "" }

  let read_mbr_partitions disk =
    try
      seek_lba_pos disk ~lba:0 ~pos:446;
      let parts = ref [] in
      for i = 0 to 3 do
        let part = read_mbr_partition disk in
        if Uuidm.equal part.type_guid Uuidm.nil
        then ()
        else parts := part :: !parts
      done;
      Ok (List.rev !parts)
    with
    | End_of_file ->
        Error (err_mbr_partitions err_eod)
    | Unix.Unix_error (e, _, _) ->
        Error (err_mbr_partitions (Unix.error_message e))
    | Failure e ->
        Error (err_mbr_partitions e)

  let make_volume_partition disk =
    let* disk_length = disk_length disk in
    let first_lba = 0 and last_lba = disk_length / disk.sector_size in
    let type_guid = Uuidm.max and guid = Uuidm.max in
    Ok { type_guid; guid; first_lba; last_lba; attributes = 0x0L; name = "" }

  let read_entries disk =
    let* partition_scheme = read_scheme disk in
    let* partition_scheme, partitions = match partition_scheme with
    | Some Gpt ->
        let* partitions = read_gpt_partitions disk in
        Ok (Gpt, partitions)
    | Some Mbr ->
        let* partitions = read_mbr_partitions disk in
        Ok (Mbr, partitions)
    | Some (Unpartitioned _ as scheme) ->
        let* partition = make_volume_partition disk in
        Ok (scheme, [partition])
    | None -> Error err_part_scheme_not_found
    in
    Ok (partition_scheme, partitions)

  let read_file_system_type disk partition =
    File_system.read_type disk ~lba:partition.first_lba

  (* Formatting *)

  let pp_first_lba disk ppf lba = match disk with
  | None -> Fmt.int ppf lba
  | Some disk ->
      let first_byte = lba_first_byte disk ~lba in
      Fmt.pf ppf "%d (first byte %d)" lba first_byte

  let pp_last_lba disk ppf lba = match disk with
  | None -> Fmt.int ppf lba
  | Some disk ->
      let last_byte = lba_first_byte disk ~lba + disk.sector_size in
      Fmt.pf ppf "%d (last byte %d or less)" lba last_byte

  let pp ?disk () =
    let type' p = type_of_type_guid (type_guid p) in
    Fmt.record
      [ Fmt.field "name" name Fmt.string;
        Fmt.field "type" type' pp_type;
        Fmt.field "guid" guid Uuidm.pp;
        Fmt.field "first-lba" first_lba (pp_first_lba disk);
        Fmt.field "last-lba" last_lba (pp_last_lba disk);
        Fmt.field "attributes" attributes (fun ppf v -> Fmt.pf ppf "%Lx" v); ]
end
