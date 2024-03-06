type t = string

let make prefix =
  let uuid = Uuidv7.make () in
  let id = Base32.of_uuidv7 uuid in
  let id = prefix ^ "_" ^ id in
  id

let to_string id = id
