(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Cmdliner

(* Config cli *)

let config =
  let open Cmdliner.Term.Syntax in
  Term.term_result' @@
  let docs = Manpage.s_common_options in
  let+ () = B0_std_cli.set_no_color ()
  and+ () = B0_std_cli.set_log_level ()
  and+ data_dir =
    let doc = "$(docv) is the data directory (images, etc.)" in
    let absent = "$(b,XDG_DATA_HOME)/$(tool)" in
    let doc_envs = [Cmd.Env.info "XDG_DATA_HOME"] in
    Arg.(value & opt (some B0_std_cli.dirpath) None &
         info ["data-dir"] ~absent ~doc ~docs ~doc_envs)
  and+ state_dir =
    let doc = "$(docv) is the state directory (logs, etc.)" in
    let absent = "$(b,XDG_STATE_HOME)/$(tool) directory" in
    let doc_envs = [Cmd.Env.info "XDG_STATE_HOME"] in
    Arg.(value & opt (some B0_std_cli.dirpath) None &
         info ["state-dir"] ~absent ~doc ~docs ~doc_envs)
  in
  Tool_config.discover ~data_dir ~state_dir

(* Converters *)

let data_dir_path_complete conf ~token =
  if not (String.starts_with ~prefix:"@" token)
  then Ok Arg.Completion.[files] else
  let data_dir = Tool_config.data_dir conf in
  let* files = Data_dir.files ~data_dir ~atify:true in
  let files = List.map Fpath.to_string files in
  let files = List.filter (String.starts_with ~prefix:token) files in
  Ok (List.map Arg.Completion.string files)

let data_dir_path_conv =
  let complete ctx ~token = match ctx with
  | None -> Ok Arg.Completion.[files]
  | Some conf -> data_dir_path_complete conf ~token
  in
  let completion = Arg.Completion.make ~context:config complete in
  Arg.Conv.of_conv ~completion B0_std_cli.path ~docv:"[@]PATH"

let drive_device_and_data_dir_path_conv =
  let parser s =
    let* p = Fpath.of_string s in
    Qemu.drive_device_of_path p
  in
  let pp ppf (device, path) = match device with
  | None -> Fpath.pp ppf path
  | Some device ->
      Fmt.pf ppf "%a:%s" Fpath.pp path (Qemu.drive_device_to_string device)
  in
  let complete ctx ~token = match ctx with
  | None -> Ok Arg.Completion.[files]
  | Some ctx ->
      match String.split_last ~sep:"@" token with
      | None -> data_dir_path_complete ctx ~token
      | Some ("", _) -> data_dir_path_complete ctx ~token
      | Some (p, dev) ->
          let devs =
            List.filter (String.starts_with ~prefix:dev) @@
            List.map Qemu.drive_device_to_string Qemu.drive_devices
          in
          let comp dev =
            Arg.Completion.string (String.concat "@" [p; dev])
          in
          Ok (List.map comp devs)
  in
  let completion = Arg.Completion.make ~context:config complete in
  let docv = "[@](IMAGE|DIR)[@DEVICE]" in
  Arg.Conv.make ~completion ~parser ~pp ~docv ()


let complete_inside_image ~image ~token =
  Result.join @@
  Result.map_error (fun e -> "Cannot mount image for completion") @@
  Disk_image.with_mount' ~image @@ fun ~mount_root ->
  let* paths =
    let rel = true and dotfiles = true and follow_symlinks = true
    and recurse = true in
    Os.Dir.fold ~rel ~dotfiles ~follow_symlinks ~recurse
      Os.Dir.path_list mount_root []
  in
  let prefix = String.drop_first (* / *)  1 (Fpath.to_string token) in
  let paths = List.map Fpath.to_string paths in
  Ok (List.filter (String.starts_with ~prefix) paths)

let complete_image_file ~token =
  let* path = Fpath.of_string token in
  let* split = Disk_image.cut_file path in
  match split with
  | None -> Ok Arg.Completion.[files; dirs]
  | Some (image, token) ->
      let* rel_paths = complete_inside_image ~image ~token in
      let comp rel =
        Arg.Completion.string (Fpath.to_string Fpath.(image // v rel))
      in
      Ok (List.map comp rel_paths)

let image_path_conv =
  let complete ctx ~token = match ctx with
  | None -> complete_image_file ~token
  | Some conf ->
      if not (String.starts_with ~prefix:"@" token)
      then complete_image_file ~token else
      let data_dir = Tool_config.data_dir conf in
      let* path = Fpath.of_string token in
      let rpath = Data_dir.resolve_path ~data_dir path in
      let* split = Disk_image.cut_file rpath in
      match split with
      | None ->
          (* Didn't find the file yet try to complete with contents
             of datadir *)
          let* files = Data_dir.files ~data_dir ~atify:true in
          let files = List.map Fpath.to_string files in
          let fss = List.filter (String.starts_with ~prefix:token) files in
          Ok (List.map Arg.Completion.string fss)
      | Some (image, ptoken) ->
          (* We have the image, complete inside the image *)
          let* rel_paths = complete_inside_image ~image ~token:ptoken in
          let* timage = (* Complete w.r.t. to @ image path *)
            let suff = Fpath.to_string ptoken in
            let suff_len = String.length suff - 1 (* keep / *) in
            Fpath.of_string (String.drop_last suff_len token)
          in
          let comp rel =
            Arg.Completion.string (Fpath.to_string Fpath.(timage // v rel))
          in
          Ok (List.map comp rel_paths)
  in
  let completion = Arg.Completion.make ~context:config complete in
  Arg.Conv.of_conv ~completion B0_std_cli.path ~docv:"[@][IMAGE/]PATH"

(* Cli fragments *)

let force =
  let doc = "Overwrite destination if it exists." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let dry_run =
  let doc = "Do not run commands, output them on $(b,stdout)." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let resolved =
  let doc = "Output absolute paths as resolved in the data directory." in
  Arg.(value & flag & info ["resolved"] ~doc)

let disk_partition_scheme_enum =
  Disk.Partition.["mbr", Some Mbr; "gpt", Some Gpt; "none", None]

let disk_partition_scheme_conv =
  Arg.enum ~docv:"SCHEME" disk_partition_scheme_enum

let disk_partition_scheme =
  let doc =
    Fmt.str "$(docv) is the disk image partition scheme. Must be %s."
      (Arg.doc_alts_enum disk_partition_scheme_enum)
  in
  Arg.(value & opt disk_partition_scheme_conv (Some Disk.Partition.Gpt) &
       info ["partition-scheme"] ~doc)

let image_file_system_enum = Disk_image.["exfat", Exfat; "fat32", Fat32]
let image_file_system_conv = Arg.enum ~docv:"FORMAT" image_file_system_enum
let image_file_system =
  let doc =
    Fmt.str "$(docv) is the file system of the disk image. Must be %s."
      (Arg.doc_alts_enum image_file_system_enum)
  in
  Arg.(value & opt image_file_system_conv Disk_image.Exfat & info ["fs"] ~doc)

let mem_size_conv =
  let parser s = match String.cut_first_while Char.Ascii.is_digit s with
  | "", "" -> Error "memory size can't be empty"
  | int, suff ->
      match int_of_string_opt int with
      | None -> Error "Could not parse integer"
      | Some int ->
          match String.Ascii.uppercase suff with
          | "M" | "MIB" | "MB" -> Ok (Fmt.str "%dM" int)
          | "G" | "GIB" | "GB" -> Ok (Fmt.str "%dG" int)
          | _ -> Fmt.error "Unknown size suffix %s" suff
  in
  let pp = Fmt.string in
  Arg.Conv.make ~parser ~pp ~docv:"INT{M,G}" ()

let accel =
  let doc =
    "$(docv) is the QEMU accelerator. Use $(b,tcg) if running the automatic \
     discovery fails. Other options depending on your platform include: \
     $(b,kvm) (Linux), $(b,hvf) (macOS), $(b,whpx) (Windows)."
  in
  let absent = "Determined according to guest arch, host arch and OS" in
  let docv = "ACCEL" in
  Arg.(value & opt (some string) None & info ["accel"] ~absent ~doc ~docv)

let mem_size =
  let doc =
    "$(docv) is the RAM size for the virtual machine. In megabyte or gigabytes."
  in
  Arg.(value & opt (some ~none:"4G" mem_size_conv) None & info ["mem"] ~doc)

let byte_size_conv =
  let parser s = match String.cut_first_while Char.Ascii.is_digit s with
  | "", "" -> Error "size can't be empty"
  | int, suff ->
      match int_of_string_opt int with
      | None -> Error "Could not parse integer"
      | Some int ->
          match String.Ascii.uppercase suff with
          | "K" | "KIB" | "KB" -> Ok (int * 1024)
          | "M" | "MIB" | "MB" -> Ok (int * 1024 * 1024)
          | "G" | "GIB" | "GB" -> Ok (int * 1024 * 1024 * 1024)
          | _ -> Fmt.error "Unknown size suffix %s" suff
  in
  let pp = Fmt.byte_size in
  Arg.Conv.make ~parser ~pp ~docv:"SIZE" ()

let byte_size ~default:absent =
  let doc =
    "$(docv) is the size in bytes. The following suffixes can be used to \
     multiply by the given factor: $(b,K) (1024), $(b,M) (1024^2), \
     $(b,G) (1024^3)."
  in
  let default = Result.get_ok (Arg.Conv.parser byte_size_conv absent) in
  Arg.(value & opt byte_size_conv default & info ["size"] ~doc ~absent)

let with_shutdown =
  let doc =
    "Do not invoke $(b,shutdown /p) at the end of the image \
     creation script generated for the plan." in
  Term.map Bool.not Arg.(value & flag & info ["plan-no-shutdown"] ~doc)

(* Architecture *)

let arch_conv =
  Arg.enum ~docv:"ARCH" Os.Arch.["arm64", arm64; "x86_64", x86_64]

let arch ?(opts = ["a"; "arch"]) ?doc ~absent () =
  let doc = match doc with
  | None -> "$(docv) is the CPU architecture of the guest OS"
  | Some doc -> doc
  in
  Arg.(value & opt (some arch_conv) None & info opts ~doc ~absent)

let arch_default_match_host ?opts ?doc () =
  let open Cmdliner.Term.Syntax in
  let absent = "Match host OS CPU architecture" in
  let+ arch = arch ?opts ?doc ~absent () in
  match arch with None -> Os.arch () | Some arch -> arch

(* List generic command *)

let list ~kind ~config ~resolved =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let data_dir = Tool_config.data_dir config in
  let* files = Data_dir.section_files ~data_dir kind ~rel:(not resolved) in
  Adhoc.print_paths files;
  Ok 0

let list_cmd ?doc k =
  let open Cmdliner.Term.Syntax in
  let kinds = Data_dir.section_to_string k in
  let doc = match doc with
  | Some doc -> doc
  | None -> Fmt.str "List %s of the @$(b,%s) directory" kinds kinds
  in
  let man = [
    `S Manpage.s_description;
    `P (Fmt.str "$(cmd) outputs the %s of the @$(b,%s) directory."
          kinds kinds) ]
  in
  Cmd.make (Cmd.info "list" ~doc ~man) @@
  let+ config and+ resolved in
  list ~kind:k ~config ~resolved


(* Generic errors *)

let err_file_exists_use_force f =
  Fmt.str "@[<v>%a: File exists@,Use %a to overwrite@]"
    Fpath.pp f Fmt.code "--force"

let err_not_dir p = Fmt.str "%a: Not a directory" Fpath.pp p
let err_not_file_or_dir p = Fmt.str "%a: Not a file or directory" Fpath.pp p
let err_no_such_file_or_dir p =
  Fmt.str "%a: No such file or directory" Fpath.pp p
