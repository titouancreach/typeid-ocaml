type t = { prefix : string; uuid : Uuidv7.t; suffix : string }

let validate_prefix prefix =
  if String.length prefix = 0 then Error "Prefix must be non-empty"
  else if
    not
      (BatString.for_all (fun ch -> BatChar.is_lowercase ch || ch = '_') prefix)
  then Error "Prefix must be all lowercase or underscore"
  else if
    not (BatString.for_all (fun ch -> BatChar.is_letter ch || ch = '_') prefix)
  then Error "Prefix must be [a-z]"
  else if String.starts_with ~prefix:"_" prefix then
    Error "Prefix cannot start with _"
  else if String.ends_with ~suffix:"_" prefix then
    Error "Prefix cannot end with _"
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
  try
    match BatString.rsplit str ~by:"_" with
    | prefix, suffix
      when Result.is_ok (validate_prefix prefix)
           && Result.is_ok (validate_suffix suffix) -> (
        let uuid = Base32.decode suffix in
        match uuid with
        | Some uuid -> Some { prefix; uuid; suffix }
        | None -> None)
    | _ -> None
  with
  | Not_found when Result.is_ok (validate_suffix str) ->
      let uuid = Base32.decode str in
      Option.map (fun uuid -> { prefix = ""; uuid; suffix = str }) uuid
  | Not_found -> None

let of_guid prefix uuid =
  let id = Base32.encode uuid in
  { prefix; uuid; suffix = id }

let get_uuid t = t.uuid
let get_prefix t = t.prefix
let get_suffix t = t.suffix

module Uuidv7 = Uuidv7
