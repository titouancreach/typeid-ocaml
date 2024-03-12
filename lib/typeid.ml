type t = { prefix : string; uuid : Uuidv7.t; suffix : string }

let validate_prefix prefix =
  if String.length prefix = 0 then Error "Prefix must be non-empty"
  else if not (BatString.for_all BatChar.is_lowercase prefix) then
    Error "Prefix must be all lowercase"
  else if not (BatString.for_all BatChar.is_letter prefix) then
    Error "Prefix must be [a-z]"
  else if String.length prefix > 63 then
    Error "Prefix must be less than 64 characters"
  else Ok ()

let make prefix =
  match validate_prefix prefix with
  | Error e -> failwith e
  | Ok () ->
      let uuid = Uuidv7.make () in
      let id = Base32.encode uuid in
      { prefix; uuid; suffix = id }

let validate_suffix suffix =
  if String.length suffix <> 26 then Error "Suffix must be 26 characters"
  else if not (BatString.for_all Base32.is_char_b32 suffix) then
    Error "Suffix must be base32"
  else Ok ()

let to_string = function
  | { prefix = ""; suffix; _ } -> suffix
  | { prefix; suffix; _ } -> prefix ^ "_" ^ suffix

let of_string_option str =
  match Base32.decode str with
  | Some a -> Some { prefix = ""; uuid = a; suffix = str }
  | None -> failwith "Invalid base32 string"

(* match String.split_on_char '_' str with *)
(* | [ prefix; suffix ] *)
(*   when Result.is_ok (validate_prefix prefix) *)
(*        && Result.is_ok (validate_suffix suffix) -> ( *)
(*     let uuid = Base32.decode suffix in *)
(*     match uuid with *)
(*     | Some uuid -> Some { prefix; uuid; suffix } *)
(*     | None -> None) *)
(* | [ suffix ] when Result.is_ok (validate_suffix suffix) -> *)
(*     let uuid = Base32.decode suffix in *)
(*     Option.map (fun uuid -> { prefix = ""; uuid; suffix }) uuid *)
(* | _ -> None *)

let of_guid prefix uuid =
  let id = Base32.encode uuid in
  { prefix; uuid; suffix = id }

let get_uuid t = t.uuid
let get_prefix t = t.prefix
let get_suffix t = t.suffix

module Uuidv7 = Uuidv7
