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

let ($) f g = g f

module SMap = Map.Make(String)
module SSet = Set.Make(String)

external processors_count : unit -> int = "nb_processors"

let read_process command =
  let in_channel = Unix.open_process_in command in
  let out = Std.input_all in_channel in
  ignore (Unix.close_process_in in_channel);
  ExtString.String.strip out

let read_process_lines command =
  let in_channel = Unix.open_process_in command in
  let out = Std.input_list in_channel in
  ignore (Unix.close_process_in in_channel);
  out

let with_in_file file f =
  let chan = open_in_bin file in
  try
    let res = f chan in
    close_in chan; res
  with e -> close_in chan; raise e

let with_out_file file f =
  let chan = open_out_bin file in
  try
    let res = f chan in
    close_out chan; res
  with e -> close_out chan; raise e

let set_hostname, get_hostname =
  let my_h = ref None in
  (fun h -> my_h := Some h),
  (fun () -> match !my_h with
             | None -> read_process "hostname"
             | Some h -> h
  )
