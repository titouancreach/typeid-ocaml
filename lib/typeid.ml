type t = { prefix : string; uuid : Uuidv7.t; suffix : string }

let make prefix =
  let uuid = Uuidv7.make () in
  let id = Base32.encode uuid in
  { prefix; uuid; suffix = id }

let to_string id = id.prefix ^ "_" ^ id.suffix

let of_string_option str =
  match String.split_on_char '_' str with
  | [ prefix; suffix ] ->
      let uuid = Base32.decode suffix in
      Some { prefix; uuid; suffix }
  | _ -> None

let get_uuid t = t.uuid
let get_prefix t = t.prefix
let get_suffix t = t.suffix

module Uuidv7 = Uuidv7
