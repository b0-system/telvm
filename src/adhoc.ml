(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

module Tui = struct
  type stop = unit -> unit

  type spin_scheme = string array * float
  let blink = [|"\x1b[32m●\x1b[0m"; " "|], 0.50
  let frames, frame_dur = blink

  let show_cursor () = Printf.printf "\x1B[?25h%!"
  let hide_cursor () = Printf.printf "\x1B[?25l%!"
  let clear_line () = Printf.printf "\x1B[2K\r%!"

  let spin ?(scheme = blink) ~msg () =
    let rec loop stop frames n i frame_dur =
      if !stop then (clear_line (); flush stdout) else
      begin
        Printf.printf "\r%s %s%!" frames.(i mod n) msg;
        flush stdout;
        Unix.sleepf frame_dur;
        loop stop frames n ((i + 1) mod n) frame_dur
      end
    in
    let stop = ref false in
    let stop_fun () = stop := true in
    let run () =
      if not (Unix.isatty Unix.stdout) then () else
      Os.Exit.on_sigint ~hook:show_cursor @@ fun () ->
      let frames, frame_dur = scheme in
      hide_cursor ();
      loop stop frames (Array.length frames) 0 frame_dur;
      show_cursor ()
    in
    ignore (Thread.create run ()); stop_fun
end

let print_paths paths =
  if paths = [] then () else Fmt.pr "@[<v>%a@]@." Fmt.(list Fpath.pp) paths

let pp_dry_run ppf cmd =
  (* XXX Should we get that in [more] ? [Cmd.pp_shell] is a bit too dumpy  *)
  let cmd = Cmd.to_list cmd in
  if cmd = [] then () else
  let system, args = List.hd cmd, List.tl cmd in
  let pp_arg = Cmd.pp_arg in
  let rec pp_args ppf = function
  | opt :: arg :: args when not (String.is_empty arg) && arg.[0] <> '-' ->
      Fmt.pf ppf " \\@,  @[<h>%a %a@]" pp_arg opt pp_arg arg; pp_args ppf args
  | arg :: args ->
      Fmt.pf ppf " \\@,  %a" pp_arg arg; pp_args ppf args
  | [] -> ()
  in
  Fmt.pf ppf "@[<v>%a%a@]" pp_arg system pp_args args

let tool_version cmd = match Os.Cmd.find cmd with
| None -> Ok None
| Some cmd ->
    let* text = Os.Cmd.run_out ~trim:true Cmd.(cmd % "--version") in
    let version = match String.split_first ~sep:"version" text with
    | None -> None
    | Some (_, r) ->
        let t = String.take_token r in
        if t = "" then None else (Some t)
    in
    match version with None -> Ok (Some "unknown") | Some _ as v -> Ok v

let now_rfc3339 () =
  let (t : Unix.tm) = Unix.gmtime (Unix.gettimeofday ()) in
  Fmt.str "%04d-%02d-%02d %02d:%02d:%02dZ"
    (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let scrape_arch_of_filepath file =
  (* We try to look for an arch starting from the end of the path *)
  let normalize = function '-' -> '_' | c -> Char.lowercase_ascii c in
  let p = String.map normalize (Fpath.to_string file) in
  let find arch p = Option.is_some (String.find_last ~sub:arch p) in
  if find "arm64" p || find "aarch64" p then Some Os.Arch.arm64 else
  if find "x64" p || find "x86_64" p || find "amd64" p
  then Some Os.Arch.x86_64
  else None

let get_arch_of_file ~arch file = match arch with
| Some arch -> arch
| None ->
    match scrape_arch_of_filepath file with
    | Some arch -> arch
    | None ->
        let host = Os.arch () in
        Log.warn
          (fun m -> m "%a: no architecture found using %a"
              Fpath.pp file (Fmt.st' [`Bold] Os.Arch.pp) host);
        host
