open Stdint

(* 48bits timestamp + 0111 + 12 bits counter + 10 + 30bits of counter + 32 bits of rands *)

let ver = Uint128.of_string "0b0111"
let var = Uint128.of_string "0b10"
let get_timestamp_ms () = Unix.time () *. 1000. |> Uint48.of_float

type t = Uint128.t

let make () =
  let result = Uint128.of_int 0 in
  let ts = get_timestamp_ms () in
  (* 48 bits of timestamp *)
  let result =
    Uint128.logor result (Uint128.shift_left (Uint128.of_uint48 ts) (128 - 48))
  in
  (* 4 bits of version *)
  let result = Uint128.logor result (Uint128.shift_left ver (128 - 48 - 4)) in
  (* 12 bits of random *)
  let bits_randoms_12_i32 = Int32.shift_right_logical (Random.bits32 ()) 20 in
  let bit_randoms_12_uint128 = Uint128.of_int32 bits_randoms_12_i32 in
  let result =
    Uint128.logor result
      (Uint128.shift_left bit_randoms_12_uint128 (128 - 48 - 4 - 12))
  in

  (* 2 bits of variant *)
  let result =
    Uint128.logor result (Uint128.shift_left var (128 - 48 - 4 - 12 - 2))
  in
  (* 62 bits of random *)
  let randomi64 = Int64.shift_right_logical (Random.bits64 ()) 2 in
  let randomuint64 = Uint128.of_int64 randomi64 in
  let result = Uint128.logor result randomuint64 in
  result

let to_uint128 t = t
