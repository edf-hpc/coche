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

{

  open Errors

  type rexpr =
    | Coord of int
    | Range of int * int
    | IpRange of (int * int) list
  type range_elt = rexpr list
  type range = range_elt list

  type hexpr =
    | Prefix of string
    | List of int * int * int
  type hosts_elt = hexpr list
  type hosts = hosts_elt list

  type ipv4 = int * int * int * int
  type prefix = int
  type cidr = ipv4 * prefix

  let seq_sep = ","
  let dot_sep = "."

  let string_of_rexpr = function
  | Coord i -> string_of_int i
  | Range (b, e) -> Printf.sprintf "%d-%d" b e
  | IpRange l ->
      let bl, el = List.split l in
      let bl = ExtString.String.join dot_sep (List.map string_of_int bl) in
      let el = ExtString.String.join dot_sep (List.map string_of_int el) in
      Printf.sprintf "(%s-%s)" bl el

  let string_of_range_elt r =
    let l = List.map string_of_rexpr r in
    ExtString.String.join dot_sep l

  let string_of_range r =
    let l = List.map string_of_range_elt r in
    ExtString.String.join seq_sep l

  let string_of_hexpr = function
  | Prefix prefix -> prefix
  | List (head, tail, len) ->
      Printf.sprintf "[%.*d-%.*d]" len head len tail

  let string_of_hosts_elt h =
    let l = List.map string_of_hexpr h in
    ExtString.String.join "" l

  let string_of_hosts h =
    let l = List.map string_of_hosts_elt h in
    ExtString.String.join seq_sep l

  let string_of_prefix = string_of_int
  let string_of_ipv4 (a,b,c,d) = Printf.sprintf "%d.%d.%d.%d" a b c d
  let string_of_cidr (ip,prefix) =
    Printf.sprintf "%s/%s"
      (string_of_ipv4 ip)
      (string_of_prefix prefix)

  let iprange bc ec =
    if (String.contains bc '.' <> String.contains ec '.') then
      raise Errors.Cannot_mix_different_kind_of_ranges
    else if String.contains bc '.' then
      let bc = ExtString.String.nsplit bc dot_sep in
      let ec = ExtString.String.nsplit ec dot_sep in
      if List.length bc = List.length ec then
        let bc = List.map int_of_string bc in
        let ec = List.map int_of_string ec in
        IpRange (List.combine bc ec)
      else
        raise IP_ranges_with_different_depths
    else
      Range (int_of_string bc, int_of_string ec)

  let min = 1
  let max = 254

}

let ident = ['-' 'a'-'z' 'A'-'Z' '0'-'9' ]

rule read_range buff = parse
| (['0'-'9']+ as coord) {
    let coord = int_of_string coord in
    next (Coord coord :: buff) lexbuf
  }
| '[' (['0'-'9']+ as bc) '-' (['0'-'9']+ as ec) ']' {
    let bc = int_of_string bc in
    let ec = int_of_string ec in
    next (Range (bc, ec) :: buff) lexbuf
  }
| '[' (['0'-'9' '.']+ as bc) '-' (['0'-'9' '.']+ as ec) ']' {
    next (iprange bc ec :: buff) lexbuf
  }
| _ { raise (Format_not_supported (Lexing.lexeme lexbuf)) }

and next buff = parse
| '.' { read_range buff lexbuf }
| eof { buff             }

and read_hosts buff = parse
| ident+ as prefix {
    read_hosts (Prefix prefix :: buff) lexbuf
  }
| (ident* as prefix) '[' (['0'-'9']+ as head) '-' (['0'-'9']+ as tail) ']' (ident* as suffix) {
    let len = Pervasives.max (String.length head) (String.length tail) in
    let head = int_of_string head in
    let tail = int_of_string tail in
    read_hosts (Prefix suffix :: List (head, tail, len) :: Prefix prefix :: buff) lexbuf
  }
| eof { buff }
| _ { raise (Invalid_hosts_description (Lexing.lexeme lexbuf)) }

{

  let rec length = function
  | [] -> 0
  | Coord _ :: l -> 1 + length l
  | Range _ :: l -> 1 + length l
  | IpRange c :: l -> List.length c + length l

  let range_elt s =
    let expr = List.rev (read_range [] (Lexing.from_string s)) in
    if length expr <> 4 then
      raise IP_range_with_incorrect_depth
    else
      expr

  let range s =
    let r = ExtString.String.nsplit s seq_sep in
    List.map range_elt r

  let hosts_elt s =
    let expr = read_hosts [] (Lexing.from_string s) in
    let rec cleanup = function
    | [] -> []
    | Prefix "" :: l -> cleanup l
    | h :: l -> h::(cleanup l)
    in
    List.rev (cleanup expr)

  let hosts s =
    let h = ExtString.String.nsplit s seq_sep in
    let l = List.map hosts_elt h in
    List.fold_left (fun acc -> function [] -> acc | elt -> elt :: acc) [] l

  let rec seq b e =
    if b <= e then
      let rec aux b e =
        if b = e
        then [e]
        else b::(aux (b+1) e)
      in aux b e
    else
      seq e b

  let coord_seq b e =
    if b <= e then
      let rec seq b e =
        if b = e
        then [[Coord e]]
        else [Coord b]::(seq (b+1) e)
      in seq b e
    else
      [[Coord e]]

  let rec mk_list n e =
    if n = 0
    then []
    else e::(mk_list (n-1) e)

  let mk_coords b e l subnet =
    let first =
      if subnet = b
      then List.map fst l
      else mk_list (List.length l) min
    in
    let last =
      if subnet = e
      then List.map snd l
      else mk_list (List.length l) max
    in
    let tail = List.combine first last in
    let tail = List.map (fun (e,f) ->
      if e = f
      then Coord e
      else Range (e,f))
      tail
    in Coord subnet :: tail

  let rec extract_first = function
  | [] -> [], []
  | [Range (b, e)] ->
      [Range (b, e)], [Range (min, max)]
  | Range (b,e) :: l ->
      let f, r = extract_first l in
      (Coord b :: f), (Range (b+1, e) :: r)
  | _ -> assert false

  let rec extract_last = function
  | [] -> [], []
  | [Range (b, e)] ->
      [Range (b, e)], [Range (min, max)]
  | Range (b,e) :: l ->
      let l, r = extract_last l in
      (Coord e :: l), (Range (b, e-1) :: r)
  | _ -> assert false

  let extract f = function
  | [a] -> [a] , None
  | l ->
      let h, l = f l in
      h, Some l

  let extract_first = extract extract_first
  let extract_last  = extract extract_last

  let option_to_list = function
  | Some l -> l
  | None -> []

  let append_coord coord prefix =
    if prefix = ""
    then coord
    else prefix ^ dot_sep ^ coord

  let rec expand_range_elt prefixes = function
  | [] -> prefixes
  | Coord coord :: l ->
      let coord = string_of_int coord in
      let prefixes = List.map (append_coord coord) prefixes in
      expand_range_elt prefixes l
  | Range (b, e) :: l ->
      if b = e
      then expand_range_elt prefixes (Coord b :: l)
      else
        let seq_be = coord_seq b e in
        let prefixes = List.flatten
          (List.map (expand_range_elt prefixes) seq_be)
        in expand_range_elt prefixes l
  | (IpRange ([]) | IpRange ([_])) :: _ ->
      raise Empty_IP_range
  | (IpRange ((b,e)::_)) :: _ when b > e ->
      raise IP_range_with_non_ordered_elements
  | (IpRange ((b,e)::l)) :: tl when b = e ->
      let prefixes = expand_range_elt (expand_range_elt prefixes [Coord b]) [IpRange l] in
      expand_range_elt prefixes tl
  | (IpRange ((b,e)::l)) :: tl ->
      let first =
        let prefixes = expand_range_elt prefixes [Coord b] in
        let subnets = List.map (fun (b,_) -> Range (b, max)) l in
        let real_first, rest = extract_first subnets in
        expand_range_elt prefixes real_first @ Option.map_default (expand_range_elt prefixes) [] rest
      in
      let middle =
        if b+1 < e then
          let p = expand_range_elt prefixes [Range (b+1,e-1)] in
          expand_range_elt p (List.map (fun _ -> Range (min, max)) l)
        else
          []
      in
      let last =
        let prefixes = expand_range_elt prefixes [Coord e] in
        let subnets = List.map (fun (_,e) -> Range (min, e)) l in
        let real_last, rest = extract_last subnets in
        expand_range_elt prefixes real_last @ Option.map_default (expand_range_elt prefixes) [] rest

      in
      expand_range_elt (first @ middle @ last) tl

  let rec expand_hosts_elt prefixes = function
  | [] -> prefixes
  | Prefix prefix :: tl ->
      let prefixes = List.map (fun p -> p ^ prefix) prefixes in
      expand_hosts_elt prefixes tl
  | List (head, tail, len) :: tl ->
      let seq = seq head tail in
      let seq = List.map (fun s -> [Prefix (Printf.sprintf "%.*d" len s)]) seq in
      let prefixes = List.flatten
        (List.map (expand_hosts_elt prefixes) seq) in
      expand_hosts_elt prefixes tl

  let cidr s =
    try
      Scanf.sscanf s "%u.%u.%u.%u/%u"
        (fun a b c d p -> (a,b,c,d),p)
    with _ ->
      raise (Invalid_network_description s)

  let ip s =
    try
      Scanf.sscanf s "%u.%u.%u.%u"
        (fun a b c d -> a,b,c,d)
    with _ ->
      raise (Invalid_IP_address_description s)

  let compare_ipv4 ip1 ip2 =
    Pervasives.compare (ip ip1) (ip ip2)

  let expand_range r =
    (* WARNING: We do not remove duplicates intentionally! *)
    let r = List.flatten (List.map (expand_range_elt [""]) r) in
    List.sort compare_ipv4 r

  let expand_hosts h =
    (* WARNING: We do not remove duplicates intentionally! *)
    let h = List.flatten (List.map (expand_hosts_elt [""]) h) in
    List.sort Pervasives.compare h

  let fold_hosts h =
    let h = String.concat seq_sep h in
    (* FIXME: This is temporary until coche gains the ability to natively
       fold a list of hosts
     *)
    let folded = Utils.read_process (Printf.sprintf "nodeset -f %s" h) in
    hosts (ExtString.String.strip folded)

  let expand_ip_range r =
    List.map ip (expand_range r)

  let ip_from_cidr = fst
  let prefix_from_cidr = snd

  let valid_ip (a,b,c,d) =
    let l = a::b::c::d::[] in
    try
      let _ = List.map Char.chr l in
      true
    with _ ->
      false

  let valid_cidr (c,p) =
    valid_ip c && p <= 32 && p > 0

  let bits i =
    let i = float_of_int i in
    int_of_float (floor (log i /. log 2.) +. 1.)

  let (&&) = Int32.logand
  and (||) = Int32.logor
  and (<<) = Int32.shift_left
  and (>>) = Int32.shift_right_logical
  and (!>) = Int32.to_int

  let to_int (a,b,c,d) =
    let ints = a::b::c::d::[] in
    let ints = List.map Int32.of_int ints in
    let bits = List.map2 (fun elm bit -> elm << bit) ints [24;16;8;0] in
    List.fold_left Int32.add Int32.zero bits

  let from_int i =
    let a = (i && 0xff000000l) >> 24
    and b = (i && 0xff0000l) >> 16
    and c = (i && 0xff00l) >> 8
    and d = (i && 0xffl) in
    !> a, !> b, !> c, !> d

  let block_of_cidr (addr, prefix) =
    let shift = 32 - prefix in
    let block_start = from_int ((to_int addr) >> shift << shift) in
    let mask = Int32.sub (0x1l << shift) 0x1l in
    let block_end = from_int ((to_int addr) || mask) in
    block_start, block_end

  let block_of_range_elt range =
    let rec limits blk_b blk_e = function
    | [] -> blk_b, blk_e
    | Coord coord :: l ->
        let coord = string_of_int coord in
        let blk_b = append_coord coord blk_b in
        let blk_e = append_coord coord blk_e in
        limits blk_b blk_e l
    | Range (b, e) :: l ->
        let b = string_of_int b in
        let e = string_of_int e in
        let blk_b = append_coord b blk_b in
        let blk_e = append_coord e blk_e in
        limits blk_b blk_e l
    | (IpRange s) :: l ->
        let blk_b, blk_e =
          List.fold_left
            (fun (blk_b, blk_e) (b,e) ->
              let b = string_of_int b in
              let e = string_of_int e in
              let blk_b = append_coord b blk_b in
              let blk_e = append_coord e blk_e in
              (blk_b, blk_e))
            (blk_b, blk_e)
            s
        in limits blk_b blk_e l
    in
    let blk_b, blk_e = limits "" "" range in
    ip blk_b, ip blk_e

  let block_of_range range =
    List.map block_of_range_elt range

  let merge_range = (@)
  let merge_hosts = (@)

  let compare addr1 addr2 = Pervasives.compare addr1 addr2

  let is_member addr cidr =
    let block_start, block_end = block_of_cidr cidr in
    Pervasives.(&&)
      (compare block_start addr <= 0)
      (compare addr block_end <= 0)

  let range_elt_is_member cidr range =
    let range_start, range_end = block_of_range_elt range in
    Pervasives.(&&)
      (is_member range_start cidr)
      (is_member range_end cidr)

  let are_members range cidr =
    List.for_all (range_elt_is_member cidr) range

  let is_host_member host hosts =
    let hosts = expand_hosts hosts in
    List.mem host hosts

  let in_range addr range =
    let blocks = block_of_range range in
    List.exists
      (fun (block_start, block_end) ->
       Pervasives.(&&)
         (compare block_start addr <= 0)
         (compare addr block_end <= 0)
      )
      blocks

}
