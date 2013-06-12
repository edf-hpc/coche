{

  exception Error of string

  type expr =
    | Coord of int
    | Range of int * int
    | IpRange of (int * int) list

  type range = expr list

  type ipv4 = int * int * int * int
  type prefix = int
  type cidr = ipv4 * prefix

  let string_of_expr = function
  | Coord i -> string_of_int i
  | Range (b, e) -> Printf.sprintf "%d-%d" b e
  | IpRange l ->
      let bl, el = List.split l in
      let bl = ExtString.String.join "." (List.map string_of_int bl) in
      let el = ExtString.String.join "." (List.map string_of_int el) in
      Printf.sprintf "(%s-%s)" bl el

  let string_of_range r =
    let l = List.map string_of_expr r in
    ExtString.String.join "." l

  let string_of_prefix = string_of_int
  let string_of_ipv4 (a,b,c,d) = Printf.sprintf "%d.%d.%d.%d" a b c d
  let string_of_cidr (ip,prefix) =
    Printf.sprintf "%s/%s"
      (string_of_ipv4 ip)
      (string_of_prefix prefix)

  let dot = Str.regexp "\\."

  let iprange bc ec =
    if (String.contains bc '.' <> String.contains ec '.') then
      raise (Error "Cannot mix ranges and IP ranges")
    else if String.contains bc '.' then
      let bc = Str.split dot bc in
      let ec = Str.split dot ec in
      if List.length bc = List.length ec then
        let bc = List.map int_of_string bc in
        let ec = List.map int_of_string ec in
        IpRange (List.combine bc ec)
      else
        raise (Error "IP ranges with different depth")
    else
      Range (int_of_string bc, int_of_string ec)

  let min = 1
  let max = 254

}

rule read buff = parse
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
| _ { raise (Error "Not supported format!") }

and next buff = parse
| '.' { read buff lexbuf }
| eof { buff             }

{

  let rec length = function
  | [] -> 0
  | Coord _ :: l -> 1 + length l
  | Range _ :: l -> 1 + length l
  | IpRange c :: l -> List.length c + length l

  let range s =
    let expr = List.rev (read [] (Lexing.from_string s)) in
    if length expr <> 4 then
      raise (Error "IP address range with length different from 4")
    else
      expr

  let seq b e =
    if b <= e then
      let rec seq b e =
        if b = e
        then [e]
        else b::(seq (b+1) e)
      in seq b e
    else
      raise (Error "Malformed Sequence (1)")

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

  let rec uniq = function
  | [] -> []
  | h::l ->
      if List.mem h l then
        uniq l
      else
        h :: uniq l

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

  let extract_first l = extract extract_first l
  let extract_last  l = extract extract_last  l

  let option_to_list = function
  | Some l -> l
  | None -> []

  let append_coord coord prefix =
    if prefix = ""
    then coord
    else prefix ^ "." ^ coord

  let rec expand' prefixes = function
  | [] -> prefixes
  | Coord coord :: l ->
      let coord = string_of_int coord in
      let prefixes = List.map (append_coord coord) prefixes in
      expand' prefixes l
  | Range (b, e) :: l ->
      if b = e
      then expand' prefixes (Coord b :: l)
      else
        let seq_be = coord_seq b e in
        let prefixes = List.flatten
          (List.map (expand' prefixes) seq_be)
        in expand' prefixes l
  | (IpRange ([]) | IpRange ([_])) :: _ ->
      raise (Error "Empty IpRange")
  | (IpRange ((b,e)::_)) :: _ when b > e ->
      raise (Error "IpRange with non-ordered elements")
  | (IpRange ((b,e)::l)) :: tl when b = e ->
      expand' (expand' prefixes [Coord b]) [IpRange l]
  | (IpRange ((b,e)::l)) :: tl ->
      let first =
        let prefixes = expand' prefixes [Coord b] in
        let subnets = List.map (fun (b,_) -> Range (b, max)) l in
        let real_first, rest = extract_first subnets in
        expand' prefixes real_first @ expand' prefixes (option_to_list rest)
      in
      let middle =
        if b+1 < e then
          let p = expand' prefixes [Range (b+1,e-1)] in
          expand' p (List.map (fun _ -> Range (min, max)) l)
        else
          []
      in
      let last =
        let prefixes = expand' prefixes [Coord e] in
        let subnets = List.map (fun (_,e) -> Range (min, e)) l in
        let real_last, rest = extract_last subnets in
        expand' prefixes real_last @ expand' prefixes (option_to_list rest)

      in
      expand' (first @ middle @ last) tl

  let cidr s =
    try
      Scanf.sscanf s "%u.%u.%u.%u/%u"
        (fun a b c d p -> (a,b,c,d),p)
    with _ ->
      raise (Error "Malformed network description")

  let ip s =
    try
      Scanf.sscanf s "%u.%u.%u.%u"
        (fun a b c d -> a,b,c,d)
    with _ ->
      raise (Error "Malformed IP address description")

  let expand_range r =
    List.sort Pervasives.compare (expand' [""] r)

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

  let block_of_range range =
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

  let compare addr1 addr2 = Pervasives.compare addr1 addr2

  let is_member addr cidr =
    let block_start, block_end = block_of_cidr cidr in
    Pervasives.(&&)
      (compare block_start addr < 0)
      (compare addr block_end < 0)

  let are_members range cidr =
    let range_start, range_end = block_of_range range in
    Pervasives.(&&)
      (is_member range_start cidr)
      (is_member range_end cidr)

}
