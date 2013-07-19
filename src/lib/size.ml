open Errors

type t = {
  size: float;
  multiplier: float;
  base10: bool;
  unit: float;
}

let value s =
  s.size *. s.multiplier *. s.unit

let rex = Pcre.regexp "^(\\d+(\\.\\d{0,2})?)\\h*([kMGTPEZY]?i?[bB])$"

let seq b e =
  let rec seq b e =
    if b = e
    then [e]
    else b::(seq (b+1) e)
  in seq b e

let powers = ['k'; 'M'; 'G'; 'T'; 'P'; 'E'; 'Z'; 'Y']

let multiplier_of_char m base10 =
  if List.mem m powers then
    let seq = seq 1 (List.length powers) in
    let powers = List.combine powers seq in
    let p = float_of_int (List.assoc m powers) in
    if base10 then
      10. ** (3. *. p) 
    else
      2. ** (10. *. p)
  else
    raise (Invalid_size_multiplier m)

let string_of_mutlplier m base10 =
  let seq = seq 1 (List.length powers) in
  let powers = List.combine seq powers in
  if m = 1. then
    ""
  else
    let p, f =
      if base10
      then (ceil (log m /. log 10.)) /. 3., ExtString.String.of_char
      else (ceil (log m /. log 2.)) /. 10., (Printf.sprintf "%ci")
    in
    f (List.assoc (int_of_float p) powers)

let unit_of_char u =
  if u = 'b' then 1. else 8.

let char_of_unit u =
  if u = 1. then 'b' else 'B'

let make n =
  try
    { size = floor (float_of_string n); multiplier = 1.; base10 = false; unit = 1. }
  with _ ->
    if Pcre.pmatch ~rex n then
      let substrings = Pcre.extract ~rex n in
      let size = float_of_string substrings.(1) in
      let unit = substrings.(3) in
      let size, multiplier, base, unit =
        match String.length unit with
          | 1 -> floor size, 1., true, (if unit.[0] = 'B' then 8. else 1.)
          | 2 -> size, multiplier_of_char unit.[0] true, true, (if unit.[1] = 'B' then 8. else 1.)
          | 3 -> size, multiplier_of_char unit.[0] false, false, (if unit.[2] = 'B' then 8. else 1.)
          | _ -> assert false
      in
      { size = size; multiplier = multiplier; base10 = base; unit = unit }
    else
      raise (Invalid_size_description n)
    
let compare n1 n2 =
  let v1 = value n1 in
  let v2 = value n2 in
  Pervasives.compare v1 v2

let to_string n =
  Printf.sprintf "%.2f %s%c"
    n.size
    (string_of_mutlplier n.multiplier n.base10)
    (char_of_unit n.unit)
