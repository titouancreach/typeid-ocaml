open Stdint

(* 48bits timestamp + 0111 + 12 bits counter + 10 + 30bits of counter + 32 bits of rands *)

type t = Uint128.t

let ver = Uint128.of_string "0b0111"
let var = Uint128.of_string "0b10"
let get_timestamp_ms () = Unix.time () *. 1000. |> Uint48.of_float

let make () =
  let result = Uint128.of_int 0 in
  let ts = get_timestamp_ms () in
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

let to_uint128 t = t
