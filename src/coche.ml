(****************************************************************************)
(*  Copyright (C) 2013-2015 EDF S.A.                                        *)
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

let usage_msg = Printf.sprintf "%s subcommand [options]\n%s"
  (Filename.basename Sys.argv.(0))
  (Subcommand.help ())

let spec = ref (
  Arg.align [
    "-verbose", Arg.Set Flags.verbose, " Enable verbose mode";
  ])

let () = Arg.parse_dynamic
  spec
  (fun arg ->
    if !Arg.current = 1 then begin
      try
        let sc = Subcommand.get_by_name (String.lowercase arg) in
        Subcommand.selected_sc := Some sc;
        spec := Arg.align (!spec @ sc.Subcommand.spec)
      with Not_found ->
        raise (Arg.Bad ("unknown subcommand " ^ arg))
    end
    else match !Subcommand.selected_sc with
    | None -> ()
    | Some sc -> sc.Subcommand.anon arg
  )
  usage_msg

let main =
  try
    begin match !Subcommand.selected_sc with
      | None -> Arg.usage !spec usage_msg
      | Some sc -> sc.Subcommand.main ()
    end
  with
  | Errors.Error e ->
     Errors.exit e
  | e ->
     Printf.eprintf "%s\n" (Printexc.to_string e)
