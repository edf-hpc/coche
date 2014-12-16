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

open Errors

type t = {
  size: float;
  multiplier: float;
}

let value s =
  s.size *. s.multiplier

let rex = Pcre.regexp "^(\\d+(\\.\\d+)?)\\h*(da|[hkMGTPEZY])?Hz$"

let seq b e =
  let rec seq b e =
    if b = e
    then [e]
    else b::(seq (b+1) e)
  in seq b e

let powers = ['d', 1; 'h',2; 'k',3; 'M',6; 'G',9; 'T',12; 'P',15; 'E',18; 'Z',21; 'Y',24]

let multiplier_of_char m =
  try
    let p = float_of_int (List.assoc m powers) in
    10. ** p
  with _ ->
    raise (Invalid_freq_multiplier m)

let string_of_mutlplier m =
  let powers = List.map (fun (u,p) -> p,u) powers in
  if m = 1. then
    ""
  else
    let p = int_of_float (ceil (log m /. log 10.)) in
    ExtString.String.of_char (List.assoc p powers)

let make n =
  try
    { size = floor (float_of_string n); multiplier = 1. }
  with _ ->
    if Pcre.pmatch ~rex n then
      let substrings = Pcre.extract ~rex n in
      let size = float_of_string substrings.(1) in
      let unit = substrings.(3) in
      let size, multiplier =
        match String.length unit with
          | 0 -> floor size, 1.
          | 1 -> size, multiplier_of_char unit.[0]
          | _ -> assert false
      in
      { size = size; multiplier = multiplier }
    else
      raise (Invalid_freq_description n)
    
let compare n1 n2 =
  let v1 = value n1 in
  let v2 = value n2 in
  Pervasives.compare v1 v2

let to_string n =
  Printf.sprintf "%.2f %sHz"
    n.size
    (string_of_mutlplier n.multiplier)
