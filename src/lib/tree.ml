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

type 'a t =
  | Empty
  | Node of 'a * 'a t list

let rec cardinal = function
  | Empty -> 0
  | Node (_, l) -> List.fold_left (fun acc x -> acc + cardinal x) 1 l

let rec make t elm = match t with
  | Empty -> Node (elm, [])
  | Node (e, l) when List.length l < !Flags.par_level ->
    Node (e, Node (elm,[])::l)
  | Node (e, l) ->
    let l = List.map (fun e -> e, cardinal e) l in
    let l = List.sort (fun (_, a1) (_, a2) -> compare a1 a2) l in
    let l = List.map fst l in
    begin
      match l with
	| a::q -> Node (e, (make a elm)::q)
	| _ -> assert false (* List.length l >= !Flags.par_level *)
    end

let make l = List.fold_left make Empty l

let walk = function
  | Empty -> None
  | Node (e, l) -> Some (e, l)
