open Printf
open Ocamlbuild_plugin

let name = "ben"
let packages = [
  "extlib";
  "xmlm";
  "pcre";
]

exception Require_findlib
exception Missing_findlib_package of string
exception Subprocess_died_unexpectedly of Unix.process_status

let try_exec cmd =
  Sys.command (sprintf "%s >/dev/null 2>&1" cmd) = 0

let require pkg =
  if not (try_exec (sprintf "ocamlfind query %s" pkg)) then
    raise (Missing_findlib_package pkg)

let ocamlfind x = S[A"ocamlfind"; A x]
let has_ocamlopt = try_exec "which ocamlopt"
let best =
  try Sys.getenv "OCAMLBEST"
  with Not_found -> if has_ocamlopt then "native" else "byte"
let () = if not (try_exec "ocamlfind printconf") then raise Require_findlib
let () = List.iter require packages
let main_executable = sprintf "%s.%s" name best

let () =
  dispatch begin function

    | Before_options ->
        Options.ocamlc := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc";
        (* Options.use_menhir := true; *)
        (* Options.ocaml_yaccflags := ["--explain"] *)

    | After_rules ->
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";
        List.iter
          (fun pkg ->
             let flag x = flag (x::["ocaml"]) & S[A"-package"; A pkg] in
             List.iter flag ["ocamldep"; "compile"; "link"; "doc"])
          packages;

        (* C stubs *)
        (* flag ["link"; "program"; "ocaml"; "byte"; "use_libdochelp"] *)
          (* (S[A"-dllib"; A"-ldochelp"]); *)
        (* dep  ["link"; "ocaml"; "use_libdochelp"] ["src/libdochelp.a"]; *)

    | _ -> ()
  end
