(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let pp_version = Fmt.(option ~none:(any "<unavailable>") string)

let pp_tool_version ppf tool =
  let version = Log.if_error ~use:None (Adhoc.tool_version tool) in
  let field = Fpath.to_string (Cmd.get_tool tool |> Result.get_ok') in
  (Fmt.field field Fun.id pp_version) ppf version

let pp_winvos_version ~data_dir ppf arch =
  let version =
    Log.if_error ~use:None (Winvos.installed_version ~data_dir ~arch)
  in
  let field = Fmt.str "winvos-%a" Os.Arch.pp arch in
  (Fmt.field field Fun.id pp_version) ppf version

let pp_qemu_version ppf guest_arch =
  let system = Qemu.system ~guest_arch |> Result.get_ok' in
  let version = Log.if_error ~use:None (Qemu.system_version system) in
  let field = Fpath.to_string (Cmd.get_tool system |> Result.get_ok') in
  (Fmt.field field Fun.id pp_version) ppf version

let pp_conf ppf conf =
  let data_dir = Telvm_conf.data_dir conf in
  let host_arch = Os.arch () in
  let host_os = Os.name () in
  let host_version = Os.version () in
  Fmt.pf ppf "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
    pp_tool_version (Cmd.tool "cabextract")
    Telvm_conf.pp conf
    (Fmt.field "host-arch" Fun.id Os.Arch.pp) host_arch
    (Fmt.field "host-os" Fun.id Os.Name.pp) host_os
    (Fmt.field "host-os-id" Fun.id Os.Name.pp_id) host_os
    (Fmt.field "host-os-version" Fun.id Fmt.string) host_version
    (Fmt.list pp_qemu_version) Qemu.guest_archs
    (Fmt.list (pp_winvos_version ~data_dir)) Winvos.archs

let output_conf ~conf = Fmt.pr "@[%a@]@." pp_conf conf; 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Output the configuration of $(tool)" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs the configuration of $(tool) on $(b,stdout)." ]
  in
  let envs = Telvm_cli.conf_envs in
  Cmd.make (Cmd.info "conf" ~doc ~man ~envs) @@
  let+ conf = Telvm_cli.conf in
  output_conf ~conf
