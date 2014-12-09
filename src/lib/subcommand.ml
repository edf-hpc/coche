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

module Arg = CocheArg

type t = {
  name: string;
  usage: string;
  description: string;
  main: unit -> unit;
  spec: (Arg.key * Arg.spec * Arg.doc) list;
  anon: string -> unit;
}

let subcommands = ref []
let selected_sc = ref (None : t option)

let register s =
  subcommands := (s.name, s) :: !subcommands

let help () =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "Available subcommands:\n";
  List.iter (fun (name,sc) ->
    let temp = Printf.sprintf "  %s %s\t\t%s\n" name sc.usage sc.description in
    Buffer.add_string buffer temp
    )
    !subcommands;
  Buffer.add_string buffer "Available options:";
  let contents = Buffer.contents buffer in
  Buffer.reset buffer;
  contents

let get_by_name s =
  List.assoc s !subcommands
