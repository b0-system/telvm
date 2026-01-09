(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_http
open Result.Syntax

let err_unsupported ctx ~arch =
  Fmt.error "%s %a: Unsupported architecture" ctx Os.Arch.pp arch

(* Upstream data *)

let virtio_win_iso =
  let filename = "virtio-win.iso" in
  let url = "https://fedorapeople.org/groups/virt/virtio-win/\
             direct-downloads/latest-virtio/virtio-win.iso"
  in
  Data_file.make Telvm ~filename ~src:(Url url)

let get_or_download_virtio_win_iso ~data_dir =
  Data_file.get_or_download ~data_dir virtio_win_iso

type arch_data =
  { arch : Os.Arch.t;
    iso : Data_file.t;
    powershell_zip : Data_file.t; }

let arm64_data =
  let iso_url = "https://aka.ms/DownloadValidationOS_arm64" in
  let pwsh_url =
    "https://github.com/PowerShell/PowerShell/releases/download/\
     v7.5.4/PowerShell-7.5.4-win-arm64.zip"
  in
  { arch = Os.Arch.arm64;
    iso = Data_file.make Telvm ~filename:"winvos-arm64.iso" ~src:(Url iso_url);
    powershell_zip =
      Data_file.make Telvm ~filename:"powershell-arm64.zip"
        ~src:(Url pwsh_url) }

let x86_64_data =
  let iso_url = "https://aka.ms/DownloadValidationOS" in
  let pwsh_url =
    "https://github.com/PowerShell/PowerShell/releases/download/\
     v7.5.4/PowerShell-7.5.4-win-x64.zip"
  in
  { arch = Os.Arch.x86_64;
    iso =
      Data_file.make Telvm ~filename:"winvos-x86_64.iso" ~src:(Url iso_url);
    powershell_zip =
      Data_file.make Telvm ~filename:"powershell-x86_64.zip"
        ~src:(Url pwsh_url) }

let archs_data = [arm64_data; x86_64_data]
let archs = List.map (fun d -> d.arch) archs_data
let is_arch arch data = Os.Arch.equal arch data.arch

let iso ~arch = match List.find_opt (is_arch arch) archs_data with
| Some data -> Ok data.iso
| None -> err_unsupported "WinVOS iso" ~arch

let find_iso_file ~data_dir ~arch =
  let* iso = iso ~arch in
  Data_file.find ~data_dir iso

let get_or_download_iso ~data_dir ~arch =
  let* iso = iso ~arch in
  Data_file.get_or_download ~data_dir iso

let powershell_zip ~arch = match List.find_opt (is_arch arch) archs_data with
| Some data -> Ok data.powershell_zip
| None -> err_unsupported "powershell zip" ~arch

let get_or_download_powershell_zip ~data_dir ~arch =
  let* powershell_zip = powershell_zip ~arch in
  Data_file.get_or_download ~data_dir powershell_zip

let copy_winvos_vhdx ~force ~make_path ~winvos_mount ~dst =
  let src = Fpath.(winvos_mount / "ValidationOS.vhdx") in
  Os.File.copy ~mode:0o644 ~make_path ~force src ~dst

(* Version *)

let parse_version_from_url url =
  let err () = Fmt.error "Could not parse version from %S" url in
  match String.split_last ~sep:"/" url with
  | None -> err ()
  | Some (_, last_seg) ->
      match String.split_all ~sep:"." last_seg with
      | _ :: _ :: vstart :: _ -> Ok vstart
      | _ -> err ()

let installed_version ~data_dir ~arch = match find_iso_file ~data_dir ~arch with
| Error _ as e -> e
| Ok None as v -> v
| Ok Some file ->
    let* url = Os.File.read Fpath.(file + ".url") in
    Result.map Option.some (parse_version_from_url url)

let upstream_version ~arch =
  let* httpc = Http_client.make ~progress:false () in
  let* data = iso ~arch in
  let Url url = Data_file.src data in
  let* final_url = Http_client.head_request_follow_location httpc url in
  parse_version_from_url final_url

(* Packages *)

let cab_extract_file ~force ~make_path ~cab ~file ~dst_dir =
  let* cabextract = Os.Cmd.get (Cmd.tool "cabextract") in
  let cmd = Cmd.(cabextract % "-p" %% path cab % "-F" % Fmt.str "*/%s" file) in
  let out_file = Fpath.(dst_dir / file) in
  let stdout = Os.Cmd.out_file ~force ~make_path:false out_file in
  Os.Cmd.run cmd ~stdout

module Pkg = struct
  type t = { name : string; files : Fpath.t list }

  let pkgs_of_cab_files files =
    let add_file acc file =
      let name = Fpath.basename ~drop_exts:true file in
      String.Map.add_to_list name file acc
    in
    let map = List.fold_left add_file String.Map.empty files in
    let make_pkg name files acc = { name; files } :: acc in
    String.Map.fold make_pkg map []

  let by_name p0 p1 = String.compare p0.name p1.name

  let pkg_list ~mount_root ~names ~add_matching_wow64 =
    let cabs = "cabs" in
    let kind = `Files in
    let* cab_files =
      let rel = true and dotfiles = false and follow_symlinks = true
      and recurse = true in
      Os.Dir.contents
        ~kind ~rel ~dotfiles ~follow_symlinks ~recurse Fpath.(mount_root / cabs)
    in
    let cab_files = List.map (Fpath.append (Fpath.v cabs)) cab_files in
    let pkgs = pkgs_of_cab_files cab_files in
    let pkgs = List.sort by_name pkgs in
    if names = [] then Ok pkgs else
    (* Note it's important to keep names in order *)
    let add_name (acc, not_found) n =
      match List.find_opt (fun p -> p.name = n) pkgs with
      | None -> acc, n :: not_found
      | Some pkg ->
          let wow64 =
            if not add_matching_wow64 then None else
            let wow64_pkg =
              String.replace_last ~sub:"-Package" ~by:"-WOW64-Package" n
            in
            List.find_opt (fun p -> p.name = wow64_pkg) pkgs
          in
          match wow64 with
          | None -> pkg :: acc, not_found
          | Some wow64 -> wow64 :: pkg :: acc, not_found
    in
    let select, not_found = List.fold_left add_name ([], []) names in
    if not_found = [] then Ok (List.rev select) else
    Fmt.error "@[<v>These packages could not be found:@,%a"
      Fmt.(list string) (List.sort String.compare not_found)

  let name pkg = pkg.name
  let files pkg = pkg.files
  let list ~data_dir ~arch ~names ~add_matching_wow64 =
    let* image = get_or_download_iso ~data_dir ~arch in
    Disk_image.with_mount ~image @@ fun ~mount_root ->
    pkg_list ~mount_root ~names ~add_matching_wow64

  let find name pkgs = List.find_opt (fun p -> String.equal name p.name) pkgs

  let contents ~data_dir ~arch ~names ~add_matching_wow64 =
    let* cabextract = Os.Cmd.get (Cmd.tool "cabextract") in
    let* image = get_or_download_iso ~data_dir ~arch in
    Disk_image.with_mount ~image @@ fun ~mount_root ->
    let* pkgs = pkg_list ~mount_root ~names ~add_matching_wow64 in
    let abs f = Fpath.append mount_root f in
    let cabs = List.concat_map (fun p -> List.map abs p.files) pkgs in
    let list_files = Cmd.(cabextract % "--list" %% paths cabs) in
    Os.Cmd.run list_files
end


module Plan = struct
  let imager_build_script ~virtio_arch =
  (* This used to be generated by a .plan file, eventually do that again *)
    let drivers =
      Fmt.str
{|/driver:%%DRIVERS%%\viostor\w11\%s ^
/driver:%%DRIVERS%%\vioscsi\w11\%s ^
/driver:%%DRIVERS%%\viogpudo\w11\%s ^
/driver:%%DRIVERS%%\NetKVM\w11\%s
|}
     virtio_arch virtio_arch virtio_arch virtio_arch
    in
{|
:: Drive where the resulting image is written
SET DST=D:

:: Mount of Validation OS ISO content
SET SRC=E:

:: Mount of the VirtIO drivers ISO
SET DRIVERS=F:

:: Create mount point
SET MOUNT=%temp%\telvm-%random%
mkdir %MOUNT%

:: DISM tool
SET DISM=%SRC%\GenImage\Tools\DISM\amd64\dism.exe

SET IMAGE=%SRC%\ValidationOS.wim

%DISM% /Mount-Image /ImageFile:%IMAGE% /Index:1 /MountDir:%MOUNT%

%DISM% /Image:%MOUNT% /Add-Package ^
/PackagePath:%SRC%\cabs\neutral\Microsoft-WinVOS-Connectivity-Package.cab ^
/PackagePath:%SRC%\cabs\en-us\Microsoft-WinVOS-Connectivity-Package.cab ^
/PackagePath:%SRC%\cabs\Extra\neutral\Microsoft-OneCore-SerialConsole-Package.cab ^
/PackagePath:%SRC%\cabs\Extra\en-us\Microsoft-OneCore-SerialConsole-Package.cab ^
/PackagePath:%SRC%\cabs\neutral\Microsoft-WinVOS-Provisioning-Package.cab ^
/PackagePath:%SRC%\cabs\en-us\Microsoft-WinVOS-Provisioning-Package.cab ^
/PackagePath:%SRC%\cabs\neutral\Microsoft-WinVOS-Filesystems-Package.cab ^
/PackagePath:%SRC%\cabs\en-us\Microsoft-WinVOS-Filesystems-Package.cab ^
/PackagePath:%SRC%\cabs\neutral\Microsoft-WinVOS-DiskTools-Package.cab ^
/PackagePath:%SRC%\cabs\en-us\Microsoft-WinVOS-DiskTools-Package.cab

%DISM% /Image:%MOUNT% /Add-Driver /ForceUnsigned ^
|} ^ drivers ^
{|
%DISM% /Unmount-Image /MountDir:%MOUNT% /commit
rmdir %MOUNT%

%DISM% /Apply-Image /ImageFile:%IMAGE% /Index:1 /ApplyDir:%DST%\

:: Make an admin user on first boot, needed for SAC console login
reg load HKLM\Offline %DST%\Windows\System32\Config\SYSTEM
reg add "HKLM\Offline\Setup" /v CmdLine /t REG_SZ /d ^
    "cmd.exe /c \"net user admin 1234 /add ^&^& net localgroup administrators admin /add\"" ^
    /f
reg add "HKLM\Offline\Setup" /v SetupType /t REG_DWORD /d 2 /f
reg unload HKLM\Offline

:: Imager script execution on boot by appending to /UserInit
reg load HKLM\Offline %DST%\Windows\System32\Config\SOFTWARE
for /f "tokens=2* skip=2" %%A in ('reg query "HKLM\Offline\Microsoft\Windows NT\CurrentVersion\Winlogon" /v Userinit') do (
    reg add "HKLM\Offline\Microsoft\Windows NT\CurrentVersion\Winlogon" ^
        /v Userinit /t REG_SZ /d "%%B, E:\create-image.cmd" /f
)
reg unload HKLM\Offline
|}

  let build_script_x86_64 =
    ":: Create an X86-64 telvm imager" ^
    (imager_build_script ~virtio_arch:"amd64") ^
{|
:: Configure boot files
bcdboot %DST%\Windows /s %DST% /f ALL

:: Enable SAC console
bcdedit  /store %DST%\EFI\Microsoft\Boot\BCD ^
             /emssettings emsport:1 emsbaudrate:115200
bcdedit  /store %DST%\EFI\Microsoft\Boot\BCD /ems on
bcdedit  /store %DST%\EFI\Microsoft\Boot\BCD /bootems on

:: Goodbye
shutdown /p
|}

  let build_script_arm64 =
    ":: Create an ARM64 telvm imager" ^
    (imager_build_script ~virtio_arch:"arm64") ^
{|
:: Configure boot files
bcdboot.exe %DST%\Windows /s %DST% /f UEFI

:: Goodbye
shutdown /p
|}

  let imager_build_scripts =
    [ Os.Arch.x86_64, build_script_x86_64;
      Os.Arch.arm64, build_script_arm64 ]

  let imager_build_script_src ~bootstrap_arch:_ ~imager_arch =
    let find (arch, _) = Os.Arch.equal imager_arch arch in
    Option.map snd @@ List.find_opt find imager_build_scripts

  let base_src =
 {|# Telvm %%VERSION%% WinVOS base
version = 1

[create]

copy-dirs =
  powershell \Windows\System32\powershell

[create winvos]

add-wow64-packages = false
copy-boot-files = false
append-to-path = %systemroot%\System32\powershell

packages =
  Microsoft-OneCore-SerialConsole-Package # Serial console
  Microsoft-WinVOS-Connectivity-Package   # USB drivers for GUI input
  Microsoft-WinVOS-Filesystems-Package    # Ability to mount UDF ISOs
  Microsoft-WinVOS-RemoteFS-Package       # Mount smb

virtio-drivers =
  viostor  # Block storage driver
  vioscsi  # For optical drives
  viogpudo # GPU pass
  viofs    # Virtio-fs driver
  NetKVM   # Network access

[create user admin]
password = 1234
groups = administrators
|}
end

(* Imager  *)

module Imager = struct

  let get_bootstrap_arch () = Os.Arch.x86_64 (* Our only choice for now *)

  (* Imager build scripts *)

  let get_build_script_src ~data_dir ~bootstrap_arch ~imager_arch = function
  | Some file ->
      let file = Data_dir.resolve_path ~data_dir file in
      Os.File.read file
  | None ->
      match Plan.imager_build_script_src ~bootstrap_arch ~imager_arch with
      | Some script -> Ok script
      | None ->
          Fmt.error "No %a bootstrap script for imager architecture %a"
            Os.Arch.pp bootstrap_arch Os.Arch.pp imager_arch

  (* Imager

     The manual has a description of the bootstrap procedure which
     may help understand these functions. Update the manual if
     you change anything. *)

  let make_imager_blank_image ~force ~make_path ~byte_size img =
    let byte_size = Byte_size.round_up_to_gb byte_size in
    (* N.B. The windows bootloader fails with GPT and/or ExFat (at least
       on macOS) formatted disks *)
    let partition_scheme = Disk.Partition.Mbr in
    let file_system = Disk_image.Fat32 in
    let name = None in
    Disk_image.make
      ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path img

  let make_bootstrap_image
      ~build_script_src ~bootstrap_winvos_iso ~bootstrap_winvos_mount
      ~imager_winvos_iso ~force img
    =
    Log.stdout (fun m -> m "Making bootstrap image");
    let with_imager_winvos_iso_mount f =
        (* If bootstrap and imager coincide we can can't mount it twice. *)
      if Fpath.equal bootstrap_winvos_iso imager_winvos_iso
      then f ~mount_root:bootstrap_winvos_mount
      else Disk_image.with_mount ~image:imager_winvos_iso f
    in
    let* () =
      let* winvos_stat = Os.Path.stat imager_winvos_iso in
      let byte_size = winvos_stat.Unix.st_size * 2 in
      let byte_size = Byte_size.round_up_to_gb byte_size in
      let partition_scheme = Disk.Partition.Gpt in
      let file_system = Disk_image.Exfat in
      let name = None and make_path = false in
      Disk_image.make
        ~partition_scheme ~file_system ~byte_size ~name ~force ~make_path img
    in
    Disk_image.with_mount ~image:img @@ fun ~mount_root:img_mount ->
    let* () =
      with_imager_winvos_iso_mount @@ fun ~mount_root:winvos_mount ->
      Log.stdout (fun m -> m "Copying WinVOS ISO files");
      let* contents =
        let dotfiles = true and follow_symlinks = false and recurse = false in
        Os.Dir.contents ~dotfiles ~follow_symlinks ~recurse winvos_mount
      in
      let cp src =
        let follow_symlinks = false and recurse = true
        and force = true and make_path = false in
        Os.Path.copy
          ~follow_symlinks ~recurse src ~force ~make_path ~dst:img_mount
      in
      List.iter_stop_on_error cp contents
    in
    let* () =
      Log.stdout (fun m -> m "Copying bcdboot.exe");
      let file = "bcdboot.exe" in
      let cab =
        Fpath.v "cabs/neutral/Microsoft-WinVOS-Provisioning-Package.cab"
      in
      let cab = Fpath.(bootstrap_winvos_mount // cab) in
      let force = true and make_path = false in
      cab_extract_file ~force ~make_path ~cab ~file ~dst_dir:img_mount
    in
    let* () =
      Log.stdout (fun m -> m "Writing bootstrap.cmd");
      let force = true and make_path = false in
      let cmd = Fpath.(img_mount / "bootstrap.cmd") in
      Os.File.write ~force ~make_path cmd build_script_src
    in
    Ok ()

  let log_bootstrap () =
    let style = [`Bold; `Fg `Green] in
    Log.stdout (fun m -> m "Starting bootstrap process…");
    Log.stdout (fun m ->
        m "@[<v>When you get to the Windows graphical cmd.exe prompt type:@,\
           @,  @[<v>%a@,%a@]@,@]"
          (Fmt.st style) "cd /d E:"
          (Fmt.st style) "bootstrap.cmd")

  let run_bootstrap
      ~accel ~mem_size ~build_script_src ~bootstrap_arch ~bootstrap_winvos_iso
      ~imager_winvos_iso ~virtio_win_iso ~dst
    =
    log_bootstrap ();
    Result.join @@ Os.File.with_tmp ~name:"vos-%s.vhdx" @@
    fun validation_os_vhdx ->
    Result.join @@ Os.File.with_tmp ~name:"bootsrap-%s.img" @@
    fun bootstrap_img ->
    let* () =
      Disk_image.with_mount ~image:bootstrap_winvos_iso @@
      fun ~mount_root:bootstrap_winvos_mount ->
      let* () =
        copy_winvos_vhdx ~force:true ~make_path:true
          ~winvos_mount:bootstrap_winvos_mount ~dst:validation_os_vhdx
      in
      make_bootstrap_image
        ~build_script_src ~bootstrap_winvos_iso ~bootstrap_winvos_mount
        ~imager_winvos_iso ~force:true bootstrap_img
    in
    let* qemu =
      let drives =
        Qemu.[Nvme, validation_os_vhdx; Nvme, dst; Nvme, bootstrap_img;
              Sata_cdrom, virtio_win_iso; ]
      in
      Qemu.cmd
        ~guest_arch:bootstrap_arch ~accel ~mem_size ~drives ~use_virtio:false
        ~use_usb_input:false ~graphic:true ~use_ramfb:false
    in
    let* () = Os.Cmd.run qemu in
    Ok ()

  let get_filepath ~data_dir ~imager_arch = function
  | Some file -> Data_dir.resolve_path ~data_dir file
  | None ->
      let dir = Data_dir.section_dir ~data_dir ~rel:false Telvm in
      let file = Fmt.str "winvos-imager-%a.img" Os.Arch.pp imager_arch in
      Fpath.(dir / file)

  let make
      ~data_dir ~accel ~mem_size ~bootstrap_arch ~build_script_src
      ~imager_arch ~force ~make_path ~dst
    =
    let warn_exists dst =
      Log.warn @@ fun m ->
      m "@[<v>Skipping bootstrap, image %a exists@,Use %a to overwrite@]"
        Fpath.pp dst Fmt.code "--force"
    in
    let dst = get_filepath ~data_dir ~imager_arch dst in
    let* dst_exists = Os.File.exists dst in
    if dst_exists && not force then (warn_exists dst; Ok ()) else
    let* virtio_win_iso = get_or_download_virtio_win_iso ~data_dir in
    let* bootstrap_winvos_iso =
      get_or_download_iso ~data_dir ~arch:bootstrap_arch
    in
    let* imager_winvos_iso = get_or_download_iso ~data_dir ~arch:imager_arch in
    let* () =
      let* winvos_stat = Os.Path.stat imager_winvos_iso in
      let byte_size = winvos_stat.Unix.st_size in
      make_imager_blank_image ~force:true ~make_path ~byte_size dst
    in
    let* () =
      run_bootstrap
        ~accel ~mem_size ~build_script_src ~bootstrap_arch ~bootstrap_winvos_iso
        ~imager_winvos_iso ~virtio_win_iso ~dst
    in
    Ok ()

  let get ~data_dir ~imager_arch file =
    let get_imager_arch file = Adhoc.get_arch_of_file ~arch:imager_arch file in
    let bootstrap_arch = get_bootstrap_arch () in
    match file with
    | Some file ->
        let imager = Data_dir.resolve_path ~data_dir file in
        let* () = Os.File.must_exist imager in
        let imager_arch = get_imager_arch imager in
        Ok (imager, imager_arch)
    | None ->
        let imager_arch = Os.arch () in
        let imager = get_filepath ~data_dir ~imager_arch None in
        let* exists = Os.File.exists imager in
        if exists then Ok (imager, get_imager_arch imager) else
        let* () =
          let* build_script_src =
            get_build_script_src ~data_dir ~bootstrap_arch ~imager_arch None
          in
          let accel = None and mem_size = None in
          make
            ~accel ~mem_size ~data_dir ~build_script_src ~bootstrap_arch
            ~imager_arch ~force:false ~make_path:true ~dst:(Some imager)
        in
        Ok (imager, imager_arch)
end
