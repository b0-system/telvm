#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "telvm" @@ fun c ->
  Ok [ Pkg.bin "src/tool/telvm_main" ~dst:"telvm";
       Pkg.mllib "src/telvm.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";]
