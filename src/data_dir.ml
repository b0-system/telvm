(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* @path handling *)

let is_at_path p = String.starts_with ~prefix:"@" (Filepath.to_string p)
let atify_path p = Filepath.v ("@" ^ (Filepath.to_string p))
let maybe_atify_path ~data_dir p =
  match Filepath.drop_strict_prefix ~prefix:data_dir p with
  | None -> p | Some rel -> atify_path rel

let resolve_path ~data_dir p =
  if not (is_at_path p) then p else
  let p = String.drop_first 1 (Filepath.to_string p) in
  if p = "" then data_dir else
  Filepath.(data_dir // Filepath.v p)

(* Data directory files *)

let files' ~dir ~rel:relp =
  let* exists = Os.Path.exists dir in
  if not exists then Ok [] else
  let rel = Option.is_some relp in
  let dotfiles = true and follow_symlinks = true and recurse = true in
  let* files =
    Os.Dir.contents ~kind:`Files ~rel ~dotfiles ~follow_symlinks ~recurse dir
  in
  match relp with
  | None -> Ok files
  | Some relp -> Ok (List.map relp files)

let files ~data_dir ~atify =
  let rel = if not atify then None else Some atify_path  in
  files' ~dir:data_dir ~rel

(* Sections *)

type section = Boot | Images | Plans | Telvm
let all_sections = [Boot; Images; Plans; Telvm]

let section_to_string = function
| Boot -> "boot" | Images -> "images" | Plans -> "plans" | Telvm -> "telvm"

let section_of_string = function
| "boot" -> Ok Boot | "images" -> Ok Images | "plans" -> Ok Plans
| "telvm" -> Ok Telvm
| s -> Fmt.error "%s: unknown data directory section" s

let section_dir ~data_dir ~rel section =
  let section = section_to_string section in
  if rel then Filepath.v ("@" ^ section) else Filepath.(data_dir / section)

let section_files ~data_dir section ~rel =
  let section = section_to_string section in
  let dir = Filepath.(data_dir / section) in
  let rel =
    if not rel then None else
    let rel_root = Filepath.v ("@" ^ section) in
    Some (fun p -> Filepath.(rel_root // p))
  in
  files' ~dir ~rel
