(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

type src =
  (* XXX These URLs should be under user control. Provide an override
     configuration file in in .config/telvm  *)
    Url of string

type t =
  { section : Data_dir.section;
    filename : string;
    src : src;
    digest : string option; }

let make ?digest section ~filename ~src = { section; filename; src; digest }
let filename data = data.filename
let src data = data.src
let digest data = data.digest

let path ~data_dir data =
  Fpath.(Data_dir.section_dir ~data_dir ~rel:false data.section / data.filename)

(* Operations *)

let download ~httpc ~data_dir data =
  let Url url = data.src in
  let open B0_http in
  (* The little silly dance here is because B0_http downloads
     to strings but these files are huge. Also we first to a HEAD
     request to get the redirected URL (because these have
     versions number we can then munge). We then download
     directly to the file without going to the Http abstraction. *)
  Log.stdout (fun m -> m "Downloading data file %a" Fmt.code data.filename);
  let file = path ~data_dir data in
  let file_url = Fpath.(file + ".url") in
  let* final_url = Http_client.head_request_follow_location httpc url in
  let make_path = true and force = true in
  Log.stdout (fun m -> m "Fetching %a" Fmt.code url);
  let args =
    Http_client.curl_fetch_args ~progress:true final_url Fpath.dash
  in
  let stdout = Os.Cmd.out_file ~make_path ~force file in
  let* () = (* XXX: Os.File.out_file needs an atomic flag. *)
    let delete_file () = ignore (Os.File.delete file) in
    Os.Exit.on_sigint ~hook:delete_file @@ fun () ->
    match Os.Cmd.run ~stdout Cmd.(tool "curl" %% args) with
    | Error _ as e -> delete_file (); e
    | Ok () -> Ok ()
  in
  let* () = Os.File.write ~make_path ~force file_url final_url in
  Ok file

let find ~data_dir data =
  let path = path ~data_dir data in
  let* exists = Os.File.exists path in
  if exists then Ok (Some path) else Ok None

let exists ~data_dir data = Result.map Option.is_some (find ~data_dir data)

let get ~data_dir data =
  let path = path ~data_dir data in
  let* () = Os.File.must_exist path in
  Ok path

let get_or_download ?(available_log = Log.Info) ?httpc ~data_dir data =
  let* httpc = match httpc with
  | Some httpc -> Ok httpc
  | None -> B0_http.Http_client.make ~progress:true ()
  in
  let* file = find ~data_dir data in
  match file with
  | None -> download ~httpc ~data_dir data
  | Some file ->
      Log.msg available_log begin fun m ->
        m "Data file %a available, skipping download" Fmt.code data.filename
      end;
      Ok file
