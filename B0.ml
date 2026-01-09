open B0_kit.V000

(* Library names *)

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let b0_std = B0_ocaml.libname "b0.std"
let threads = B0_ocaml.libname "threads"
let uuidm = B0_ocaml.libname "uuidm"
let telvm = B0_ocaml.libname "telvm"

(* Libraries *)

let telvm_lib =
  let srcs = [`Dir ~/"src"] in
  let requires = [b0_std; cmdliner; uuidm; unix; threads] in
  B0_ocaml.lib telvm ~name:"telvm-lib" ~public:true ~srcs ~requires

(* Tools *)

let telvm_tool =
  let srcs = [`Dir ~/"src/tool"] in
  let requires = [b0_std; cmdliner; uuidm; unix; telvm; threads] in
  B0_ocaml.exe "telvm" ~public:true ~srcs ~requires

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(unix :: b0_std :: telvm :: requires)

let test_disk = test ~/"test/test_disk.ml"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The telvm programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/telvm"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/telvm/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/telvm.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/telvm/issues"
    |> ~~ B0_meta.description_tags ["vm"; "qemu"; "windows"; "org:erratique"]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "5.3.0"|};
        "ocamlfind", {|build|};
        "b0", {|>= "0.0.6"|};
        "uuidm", {|>= "0.9.10"|};
        "cmdliner", {|>= "2.0.0"|};]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]
         [ "cmdliner" "install" "tool-support"
           "--update-opam-install=%{_:name}%.install"
           "_build/src/tool/telvm_main.native:telvm" {ocaml:native}
           "_build/src/tool/telvm_main.byte:telvm" {!ocaml:native}
           "_build/cmdliner-install"]]|}
    |> ~~ B0_opam.pin_depends
      [ "b0.dev", "git+https://erratique.ch/repos/b0.git#master"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~meta ~locked:true @@
  B0_unit.list ()
