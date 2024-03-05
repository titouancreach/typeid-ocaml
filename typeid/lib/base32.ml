open Stdint

let explode_string s = List.init (String.length s) (String.get s)

let pad_and_normalize s =
  match explode_string s with
  | '0' :: 'b' :: xs ->
      let padding = List.init (128 - List.length xs) (fun _ -> '0') in
      BatString.of_list (padding @ xs)
  | _ -> raise (Invalid_argument "Invalid binary string")

let encode_char = function
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  | 16 -> 'g'
  | 17 -> 'h'
  | 18 -> 'j'
  | 19 -> 'k'
  | 20 -> 'm'
  | 21 -> 'n'
  | 22 -> 'p'
  | 23 -> 'q'
  | 24 -> 'r'
  | 25 -> 's'
  | 26 -> 't'
  | 27 -> 'v'
  | 28 -> 'w'
  | 29 -> 'x'
  | 30 -> 'y'
  | 31 -> 'z'
  | _ -> raise (Invalid_argument "Invalid argument, should be a 0 and 31")

let of_uuidv7 uuid =
  let rec aux str =
    match explode_string str with
    | a :: b :: c :: d :: e :: xs ->
        let bin_str = "0b" ^ BatString.of_list [ a; b; c; d; e ] in
        let as_int = Uint32.to_int (Uint32.of_string bin_str) in
        BatString.of_char (encode_char as_int) ^ aux (BatString.of_list xs)
    | _ -> ""
  in
  let uint128 = Uuidv7.to_uint128 uuid in
  let str = Uint128.to_string_bin uint128 in
  aux (pad_and_normalize str)
