open Stdint

(* 48bits timestamp + 0111 + 12 bits counter + 10 + 30bits of counter + 32 bits of rands *)

let explode_string s = List.init (String.length s) (String.get s)

let pad_and_normalize s =
  match explode_string s with
  | '0' :: 'b' :: xs ->
      let padding = List.init (128 - List.length xs) (fun _ -> '0') in
      BatString.of_list (padding @ xs)
  | _ -> raise (Invalid_argument "Invalid binary string")

let debug_binary binary =
  let str = Uint128.to_string_bin binary in
  match explode_string str with
  | '0' :: 'b' :: xs when List.length xs < 128 ->
      let padding = List.init (128 - List.length xs) (fun _ -> '0') in
      Printf.printf "%s\n" ("0b" ^ BatString.of_list (padding @ xs))
  | _ -> Printf.printf "%s\n" str

let timestamp_ms () = Unix.time () *. 1000. |> Uint48.of_float

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

let rec encode_to_b32 str =
  match explode_string str with
  | a :: b :: c :: d :: e :: xs ->
      let bin_str = "0b" ^ BatString.of_list [ a; b; c; d; e ] in
      let as_int = Uint32.to_int (Uint32.of_string bin_str) in
      BatString.of_char (encode_char as_int)
      ^ encode_to_b32 (BatString.of_list xs)
  | _ -> ""

let ver = Uint128.of_string "0b0111"
let var = Uint128.of_string "0b10"

let uuidv7 () =
  let result = Uint128.of_int 0 in
  let ts = timestamp_ms () in
  let result =
    Uint128.logor result (Uint128.shift_left (Uint128.of_uint48 ts) (128 - 48))
  in
  let result = Uint128.logor result (Uint128.shift_left ver (128 - 48 - 4)) in
  let result =
    Uint128.logor result (Uint128.shift_left var (128 - 48 - 4 - 12 - 2))
  in
  let random = Random.bits32 () in
  let result = Uint128.logor result (Uint128.of_int32 (Int32.abs random)) in
  result

let () =
  Random.self_init ();
  for i = 0 to 10 do
    let uuid = uuidv7 () in
    let binary_string = Uint128.to_string_bin uuid in
    let value_to_encode = pad_and_normalize binary_string in
    Printf.printf "Encoded: %s \n" ("my_id" ^ encode_to_b32 value_to_encode)
  done
