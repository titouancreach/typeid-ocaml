let get_guid typeid_str =
  match Typeid.of_string_option typeid_str with
  | Some x ->
      Some "00000000-0000-0000-0000-000000000000"
      (*Some (Typeid.Uuidv7.to_string (Typeid.get_uuid x))*)
  | None -> None

let get_prefix typeid_str =
  match Typeid.of_string_option typeid_str with
  | Some x -> Some (Typeid.get_prefix x)
  | None -> None

type valid_test_case = {
  parse : unit -> unit;
  encode : unit -> unit;
  check_prefix : unit -> unit;
}

let make_valid_test_case guid_str prefix typeid_str =
  {
    parse = (fun () -> ());
    encode = (fun () -> ());
    check_prefix = (fun () -> ());
  }
