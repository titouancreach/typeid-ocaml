let get_guid typeid_str =
  match Typeid.of_string_option typeid_str with
  | Some x -> Some (Typeid.Uuidv7.to_string (Typeid.get_uuid x))
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
    parse =
      (fun () ->
        Alcotest.(check (option string))
          "Should parse correctly" (Some guid_str) (get_guid typeid_str));
    encode =
      (fun () ->
        Alcotest.(check string)
          "Should encode correctly" typeid_str
          (Typeid.to_string
             (Typeid.of_guid prefix (Typeid.Uuidv7.of_string guid_str))));
    check_prefix =
      (fun () ->
        Alcotest.(check (option string))
          "Should parse correctly" (Some prefix) (get_prefix typeid_str));
  }
