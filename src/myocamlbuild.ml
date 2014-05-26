(****************************************************************************)
(*  Copyright (C) 2013-2014 EDF S.A.                                        *)
(*                                                                          *)
(*  This file is part of Coche.                                             *)
(*                                                                          *)
(*  Coche is free software: you can redistribute it and/or modify it        *)
(*  under the terms of the GNU General Public License as published by the   *)
(*  Free Software Foundation, either version 3 of the License, or (at your  *)
(*  option) any later version.                                              *)
(*                                                                          *)
(*  Coche is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   *)
(*  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   *)
(*  for more details.                                                       *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with Coche.  If not, see <http://www.gnu.org/licenses/>.          *)
(*                                                                          *)
(****************************************************************************)

open Printf
open Ocamlbuild_plugin

let name = "ben"
let packages = [
  "unix";
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

let subcommands =
  let cmds = Array.to_list (Sys.readdir "subcommands") in
  List.map (fun subcommand -> "subcommands/" ^ (Filename.chop_extension subcommand)) cmds

let subcommands ext = List.map (fun subcommand -> subcommand ^ ext) subcommands

let cmx_subcommands = subcommands ".cmx"
let cmo_subcommands = subcommands ".cmo"
let ml_subcommands  = subcommands ".ml"

let () =
  dispatch begin function

    | Before_options ->
        Options.ocamlc := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc";
        Options.ocamlmklib := ocamlfind "ocamlmklib";

    | After_rules ->
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";
        List.iter
          (fun pkg ->
             let flag x = flag (x::["ocaml"]) & S[A"-package"; A pkg] in
             List.iter flag ["ocamldep"; "compile"; "link"; "doc"])
          packages;

        (* -lutil is needed in order to use forkpty *)
        flag ["ocamlmklib"; "c"] (S[A "-lutil"]);

        (* subcommands *)
        dep ["coche_src"] & ml_subcommands;
        dep ["coche"; "byte"] & cmo_subcommands;
        dep ["coche"; "native"] & cmx_subcommands;

        (* cocheLib *)
        flag ["ocaml"; "link"; "byte"; "use_cochelib"] & A"cocheLib.cma";
        flag ["ocaml"; "link"; "native"; "use_cochelib"] & A"cocheLib.cmxa";

        (* C stubs *)
        flag ["link"; "library"; "ocaml"; "byte"; "use_libcoche"]
          (S[A"-dllib"; A"-lcoche"; A"-cclib"; A"-lcoche"]);
        flag ["link"; "library"; "ocaml"; "native"; "use_libcoche"]
          (S[A"-cclib"; A"-lcoche"]);
        flag ["link"; "library"; "ocaml"; "native"; "use_libcoche"]
          (S[A"-cclib"; A"-lcoche"]);
        flag ["link"; "program"; "ocaml"; "byte"; "use_libcoche"]
          (S[A"-I"; A"lib"; A"-dllib"; A"-lcoche"]);
        dep  ["link"; "ocaml"; "use_libcoche"] ["lib/libcoche.a"];

    | _ -> ()
  end
