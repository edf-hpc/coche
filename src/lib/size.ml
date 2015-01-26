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

open Errors

type t = {
  size: float;       (* Size modulo multiplier and unit        *)
  multiplier: float; (* Helps to get back to the original form *)
  base10: bool;      (* Only useful to not inspect multiplier  *)
  unit: float;       (* Byte or bits, that is the question.    *)
}

let value s =
  s.size *. s.multiplier *. s.unit

let rex = Pcre.regexp "^(\\d+(\\.\\d{0,2})?)\\h*([kMGTPEZY]?i?[bB]?)$"

let powers =
 ['k', 1; 'M', 2; 'G', 3; 'T', 4; 'P', 5; 'E', 6; 'Z', 7; 'Y', 8]

let multiplier_of_char m base10 =
  try
    let p = float_of_int (List.assoc m powers) in
    if base10 then
      10. ** (3. *. p) 
    else
      2. ** (10. *. p)
  with Not_found ->
    raise (Invalid_size_multiplier m)

let string_of_mutlplier m base10 =
  if m = 1. then
    if base10 then "" else "i"
  else
    let p, suffix =
      if base10
      then (ceil (log m /. log 10.)) /. 3., ""
      else (ceil (log m /. log 2.)) /. 10., "i"
    in
    let powers = List.map (fun (c, d) -> d, c) powers in
    Printf.sprintf
      "%c%s"
      (List.assoc (int_of_float p) powers)
      suffix

let unit_of_char u =
  if u = 'b' then 1. else 8.

let char_of_unit u =
  if u = 1. then 'b' else 'B'

let make n =
  try
    let size = floor (float_of_string n) in
    let size, unit =
      if (int_of_float size) mod 8 = 0 then
        size /. 8., 8.
      else
        size, 1.
    in
    { size = size; multiplier = 1.; base10 = true; unit = unit }
  with _ ->
    if Pcre.pmatch ~rex n then
      let substrings = Pcre.extract ~rex n in
      let size = float_of_string substrings.(1) in
      let unit = substrings.(3) in
      let size, multiplier, base10, unit =
        match ExtString.String.explode unit with
        | b::[]         -> size, 1., true, (if b = 'B' then 8. else 1.)
        | 'i'::b::[]    -> size, 1., false, (if b = 'B' then 8. else 1.)
        | m::b::[]      -> size, multiplier_of_char m true, true, (if b = 'B' then 8. else 1.)
        | m::'i'::b::[] -> size, multiplier_of_char m false, false, (if b = 'B' then 8. else 1.)
        | _ -> assert false
      in
      { size = size; multiplier = multiplier; base10 = base10; unit = unit }
    else
      raise (Invalid_size_description n)

let compare n1 n2 =
  let v1 = value n1 in
  let v2 = value n2 in
  Pervasives.compare v1 v2

let to_string n =
  Printf.sprintf "%.2f%s%c"
    n.size
    (string_of_mutlplier n.multiplier n.base10)
    (char_of_unit n.unit)

let sub s1 s2 =
  match s2 with
    | None -> s1
    | Some s2 ->
       if s1.multiplier = s2.multiplier
          && s1.base10 = s2.base10
          && s1.unit = s2.unit
       then
         { s1 with size = max 0. (s1.size -. s2.size) }
       else
         let v = max 0. (value s1 -. value s2) in
         make (string_of_float v)
