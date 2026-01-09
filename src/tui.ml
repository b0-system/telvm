(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type stop = unit -> unit

type spin_scheme = string array * float
let blink = [|"\x1b[32m●\x1b[0m"; " "|], 0.50
let frames, frame_dur = blink

let show_cursor () = Printf.printf "\x1B[?25h"
let hide_cursor () = Printf.printf "\x1B[?25l"
let clear_line () = Printf.printf "\x1B[2K\r"

let spin ?(scheme = blink) ~msg () =
  let rec loop stop frames n i frame_dur =
    if !stop then (clear_line (); flush stdout) else
    begin
      Printf.printf "\r%s %s" frames.(i mod n) msg;
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
