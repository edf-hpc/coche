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

let quiet = ref false

let seq = ['|'; '/'; '-'; '\\']
let seq_len = List.length seq

type status = {
  mutable finished : int;
  mutable failed   : int;
  mutable total    : int;
}

let clear_to_eol () = Printf.fprintf stdout "\027[K";;
let bol () = Printf.fprintf stdout "\r";;

let print st =
  clear_to_eol ();
  bol ();
  let pos =
    let completed = st.finished + st.failed in
    if completed = st.total then
      '='
    else
      List.nth seq (completed mod seq_len)
  in
  Printf.printf "%c Finished (%4d)\tFailed (%4d)\tTotal (%4d)"
                pos
                st.finished
                st.failed
                st.total

let make_status finished failed total =
  { finished; failed; total }

let update ?finished:(d_finished=0) ?failed:(d_failed=0) st =
  let new_st =
    { st with
      finished = st.finished + d_finished;
      failed   = st.failed   + d_failed;
    }
  in print new_st

