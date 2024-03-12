open Stdint

let explode_string s = List.init (String.length s) (String.get s)

let pad_and_normalize s =
  match explode_string s with
  | '0' :: 'b' :: xs ->
      (* pad to 128 but add to leading 0 to mach multiple of 5*)
      let padding = List.init (130 - List.length xs) (fun _ -> '0') in
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

let decode_char = function
  | '0' -> "00000" (* 0 *)
  | '1' -> "00001" (* 1 *)
  | '2' -> "00010" (* 2 *)
  | '3' -> "00011" (* 3 *)
  | '4' -> "00100" (* 4 *)
  | '5' -> "00101" (* 5 *)
  | '6' -> "00110" (* 6 *)
  | '7' -> "00111" (* 7 *)
  | '8' -> "01000" (* 8 *)
  | '9' -> "01001" (* 9 *)
  | 'a' -> "01010" (* 10 *)
  | 'b' -> "01011" (* 11 *)
  | 'c' -> "01100" (* 12 *)
  | 'd' -> "01101" (* 13 *)
  | 'e' -> "01110" (* 14 *)
  | 'f' -> "01111" (* 15 *)
  | 'g' -> "10000" (* 16 *)
  | 'h' -> "10001" (* 17 *)
  | 'j' -> "10010" (* 18 *)
  | 'k' -> "10011" (* 19 *)
  | 'm' -> "10100" (* 20 *)
  | 'n' -> "10101" (* 21 *)
  | 'p' -> "10110" (* 22 *)
  | 'q' -> "10111" (* 23 *)
  | 'r' -> "11000" (* 24 *)
  | 's' -> "11001" (* 25 *)
  | 't' -> "11010" (* 26 *)
  | 'v' -> "11011" (* 27 *)
  | 'w' -> "11100" (* 28 *)
  | 'x' -> "11101" (* 29 *)
  | 'y' -> "11110" (* 30 *)
  | 'z' -> "11111" (* 31 *)
  | _ -> raise (Invalid_argument "Invalid argument, should be a 0 and 31")

(* fixme: not the best way *)
let is_char_b32 c =
  try
    ignore (decode_char c);
    true
  with Invalid_argument _ -> false

let decode b32_encoded_string =
  let binary_string =
    List.fold_left
      (fun acc c -> acc ^ decode_char c)
      ""
      (explode_string b32_encoded_string)
  in
  (* can throw if overflow *)
  try
    let uint128 = Uint128.of_string ("0b" ^ binary_string) in
    Some (Uuidv7.of_uint128 uint128)
  with _ -> None

let encode uuid =
  let rec aux str =
    match explode_string str with
    | a :: b :: c :: d :: e :: xs ->
        let bin_str = "0b" ^ BatString.of_list [ a; b; c; d; e ] in
        let as_int = Uint8.to_int (Uint8.of_string bin_str) in
        BatString.of_char (encode_char as_int) ^ aux (BatString.of_list xs)
    | [] -> ""
    | x ->
        let error =
          Printf.sprintf "Invalid binary string, %d char remaining"
            (List.length x)
        in
        raise (Invalid_argument error)
  in
  let uint128 = Uuidv7.to_uint128 uuid in
  let str = Uint128.to_string_bin uint128 in
  aux (pad_and_normalize str)
