(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* FIXME better errors typos, report unknown and error locations, but this
   needs to go to a good fini library *)

let fini_get_atoms ~default k doc =
  Option.value ~default:[] (B0_fini.find k doc)

let fini_get_atom k doc = match B0_fini.find k doc with
| Some [a] -> Ok a
| None | Some _ -> Fmt.error "key %a: missing required atom" B0_fini.pp_qname k

let fini_find_bool ~default k doc = match B0_fini.find k doc with
| None | Some [] -> Ok default
| Some ["true"] -> Ok true
| Some ["false"] -> Ok false
| Some _ ->
    Fmt.error "key %a: expected true or false"
      Fmt.(list ~sep:(any ".") string) k

let fini_get_int k doc = match B0_fini.find k doc with
| Some [i] when Option.is_some (int_of_string_opt i) -> Ok (int_of_string i)
| None ->
    Fmt.error "Missing required key %a" B0_fini.pp_qname k
| Some _ ->
    Fmt.error "Key %a: expected int" B0_fini.pp_qname k

(* Plans *)

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

let pp_copy_dir ppf copy = Fmt.pf ppf "@[copy %s to %s@]" copy.src copy.dst

let parse_winvos doc =
  let* add_wow64_packages =
    fini_find_bool ~default:false ["add-wow64-packages"] doc
  in
  let append_to_path = fini_get_atoms ~default:[] ["append-to-path"] doc in
  let boot_execs = fini_get_atoms ~default:[] ["boot-execs"] doc in
  let* copy_boot_files =
    fini_find_bool ~default:false ["copy-boot-files"] doc
  in
  let packages = fini_get_atoms ~default:[] ["packages"] doc in
  let virtio_drivers = fini_get_atoms ~default:[] ["virtio-drivers"] doc in
  Ok { add_wow64_packages; append_to_path; boot_execs; copy_boot_files;
       packages; virtio_drivers; }

let parse_users doc =
  let add_user acc username =
    let groups = fini_get_atoms ~default:[] [username; "groups"] doc in
    let password =
      fini_get_atom [username; "password"] doc |> Result.error_to_failure
    in
    { groups; password; username } :: acc
  in
  let users = B0_fini.top_sections doc in
  try Ok (List.rev (List.fold_left add_user [] users)) with
  | Failure e -> Error e

let parse_create doc =
  let* copy_dirs =
    let copy_dirs = fini_get_atoms ~default:[] ["copy-dirs"] doc in
    let rec loop mvs = function
    | src :: dst :: paths -> loop ({src; dst} :: mvs) paths
    | [] -> Ok (List.rev mvs)
    | [file] ->
        Fmt.error "Missing a destination directory for %s in copy-dirs" file
    in
    loop [] copy_dirs
  in
  let* users = parse_users (B0_fini.get_section ["user"] doc) in
  let* winvos = parse_winvos (B0_fini.get_section ["winvos"] doc) in
  Ok { copy_dirs; users; winvos }

let of_ini ?(file = Fpath.dash) src =
  let basename = Fpath.basename ~drop_exts:true file in
  let basename = if basename = "-" then "winvos" else basename in
  let arch = None in
  let* doc = B0_fini.of_string ~file:(Fpath.to_string file) src in
  let* version = match fini_get_int ["version"] doc with
  | Ok 1 as v -> v
  | Ok n -> Fmt.error "version: expected 1 found %d" n | Error _ as v -> v
  in
  let* create = parse_create (B0_fini.get_section ["create"] doc) in
  Ok { arch; basename; create; version; }

(* WinVOS create image command *)

let find_virtio_driver_dir ~arch driver =
  let arch = match arch with
  | Os.Arch.Arm64 _ -> "ARM64"
  | Os.Arch.X86_64 _ -> "amd64"
  | _ -> assert false
  in
  Ok (Fmt.str {|\%s\w11\%s|} driver arch)

let get_virtio_drivers ~arch plan =
  let add_driver (acc, errs) driver =
    match find_virtio_driver_dir ~arch driver with
    | Ok dir -> dir :: acc, errs
    | Error err -> acc, errs
  in
  let acc, errs =
    List.fold_left add_driver ([], []) plan.create.winvos.virtio_drivers
  in
  if errs = [] then Ok (List.rev acc) else
  Fmt.error "@[Some drivers could not be found: %a@]" Fmt.(list string) errs

let reg_append_to_path dirs =
  if dirs = [] then "" else
  let escape dir = String.replace_all ~sub:"%" ~by:"%%" dir in
  let dirs = List.map escape dirs in
  let set_value = String.concat ";" ("set value=!value!" :: dirs) in
  String.concat "\n" [ "";
{|
:: Update %PATH% in registry
reg load HKLM\Offline %MOUNT%\Windows\System32\config\SYSTEM
set key="HKLM\Offline\ControlSet001\Control\Session Manager\Environment"
for /f "tokens=2*" %%A in ('reg query %key% /v Path') do set value=%%B
:: Avoid %SystemRoot% expansion
setlocal enabledelayedexpansion|};
  set_value;
{|reg add !key! /v Path /t REG_EXPAND_SZ /d "!value!" /f
endlocal
reg unload HKLM\Offline|}]

let add_dism_tool ~arch =
  let arch = match arch with
  | Os.Arch.Arm64 _ -> "arm64"
  | Os.Arch.X86_64 _ -> "amd64"
  | _ -> assert false
  in
  let set = Fmt.str {|SET DISM=%%SRC%%\GenImage\Tools\DISM\%s\dism.exe|} arch in
  String.concat "\n" [""; {|:: DISM tool|}; set]

let dism_add_package pkgs =
  if pkgs = [] then "" else
  let package_path_arg cabfile =
    let to_win_path f = (* TODO Fpath *)
      String.concat "\\" ("" :: Fpath.to_segments f)
    in
    "/PackagePath:%SRC%" ^ (to_win_path cabfile)
  in
  let files = List.concat_map Winvos.Pkg.files pkgs in
  let packages = String.concat " ^\n" ("" :: List.map package_path_arg files) in
  String.concat "\n" ["\n"; {|%DISM% /Image:%MOUNT% /Add-Package|} ^ packages]

let dism_add_driver drivers =
  if drivers = [] then "" else
  let driver_arg dir = "/driver:%DRIVERS%" ^ dir in
  let drivers = String.concat " ^\n" ("" :: List.map driver_arg drivers) in
  String.concat "\n" ["\n";
                      {|%DISM% /Image:%MOUNT% /Add-Driver /ForceUnsigned |} ^
                      drivers]

let copy_boot_files ~arch copy =
  (* The imager image build from [.wim] need that step.
     The [.vhdx] are presumably already bootable so it's not needed. *)
  if not copy then "" else
  let bcdboot = match arch with
  | Os.Arch.X86_64 _ -> {|bcdboot %MOUNT%\Windows /s %MOUNT% /f ALL|}
  | Os.Arch.Arm64 _ -> {|bcdboot %MOUNT%\Windows /s %MOUNT% /f UEFI|}
  | _ -> assert false
  in
  String.concat "\n" ["\n"; {|:: Copy boot files|}; bcdboot ]

let enable_sac_console ~arch = match arch with
| Os.Arch.X86_64 _ ->
"\n" ^ {|
:: Enable SAC console
:: This mounts the hidden SYSTEM partition of the vhdx volume
:: TODO the disk GUID will likely change in the future version of WinVOS
:: This is brittle and should be improved.
mountvol R: \\?\Volume{c587fa46-aec7-40b5-ae44-610e51124aa2}
bcdedit /store R:\EFI\Microsoft\Boot\BCD /ems {default} on
bcdedit /store R:\EFI\Microsoft\Boot\BCD ^
               /emssettings emsport:1 emsbaudrate:115200
mountvol R: /D|}
| Os.Arch.Arm64 _ ->
"\n" ^ {|
:: Enable SAC console
:: On Arm64 nothing needs to be done|}
| _ -> assert false

let xcopy_dirs dirs =
  if dirs = [] then "" else
  let xcopy {src; dst} =
    Fmt.str {|xcopy %%PLAN%%\%s\* %%MOUNT%%%s\ /E /I /H /K /Q|} src dst
  in
  let xcopies = String.concat "\n" (List.map xcopy dirs) in
  String.concat "\n" ["\n"; {|:: Copy dirs from plan image|}; xcopies;]

let reg_first_boot ~arch users = (* generalize? make a self-deleting script ? *)
  let line = match arch with
  | Os.Arch.X86_64 _
  | Os.Arch.Arm64 _ ->
      let user u =
        let password = if u.password = "" then "''" else u.password in
        Fmt.str "net user %s %s /add" u.username password
      in
      let group u group =
        Fmt.str "net localgroup %s %s /add" group u.username
      in
      let user u = (user u) :: List.map (group u) u.groups in
      let mk_users = String.concat " ^&^& " (List.concat_map user users) in
      Fmt.str "\\\"%s\\\"" mk_users
  | _ -> assert false
  in
  let reg_add =
    {|reg add "HKLM\Offline\Setup" /v CmdLine /t REG_SZ /d ^ "cmd.exe /c |} ^
    line ^ {|" /f|}
  in
  String.concat "\n" [ "\n";
{|:: Make an admin user on first boot, needed for SAC console login
reg load HKLM\Offline %MOUNT%\Windows\System32\Config\SYSTEM|};
reg_add;
{|reg add "HKLM\Offline\Setup" /v SetupType /t REG_DWORD /d 2 /f
reg unload HKLM\Offline|};]

let reg_boot_execs boot_execs =
  if boot_execs = [] then "" else
  let execs =
    (* N.B. an insane amount of quoting is likely needed here *)
    String.concat ", " ("" :: boot_execs)
  in
  String.concat "" [ "\n";
{|:: Execute commands after boot by appending to /UserInit
:: harmless if script does not exist
reg load HKLM\Offline %MOUNT%\Windows\System32\Config\SOFTWARE
for /f "tokens=2* skip=2" %%A in ('reg query "HKLM\Offline\Microsoft\Windows NT\CurrentVersion\Winlogon" /v Userinit') do (
    reg add "HKLM\Offline\Microsoft\Windows NT\CurrentVersion\Winlogon" ^
        /v Userinit /t REG_SZ /d "%%B|}; execs; {|" /f
)
reg unload HKLM\Offline
|}]

let to_winvos_create_image_cmd ~data_dir ~arch ~with_shutdown plan =
  let* pkgs =
    let names = plan.create.winvos.packages in
    let add_matching_wow64 = plan.create.winvos.add_wow64_packages in
    Winvos.Pkg.list ~data_dir ~arch ~names ~add_matching_wow64
  in
  let* drivers = get_virtio_drivers ~arch plan in
  let copy_dirs = plan.create.copy_dirs in
  let append_to_path = plan.create.winvos.append_to_path in
  Result.ok @@ String.concat "" [
{|:: create-image - generated by telvm %%VERSION%%
@echo off
call :RUN > E:\log.txt 2>&1
exit /b

:RUN
@echo on

:: Mount of ValidationOS.vhdx
SET MOUNT=D:

:: Mount of the plan image
SET PLAN=E:

:: Mount of the Validation OS ISO
SET SRC=F:

:: Mount of the VirtIO drivers ISO
SET DRIVERS=G:
|};
add_dism_tool ~arch;
dism_add_package pkgs;
dism_add_driver drivers;
copy_boot_files ~arch plan.create.winvos.copy_boot_files;
enable_sac_console ~arch;
xcopy_dirs copy_dirs;
reg_first_boot ~arch plan.create.users;
reg_append_to_path append_to_path;
reg_boot_execs plan.create.winvos.boot_execs;
if with_shutdown then
{|
:: Goodbye
shutdown /p
|} else ""];
