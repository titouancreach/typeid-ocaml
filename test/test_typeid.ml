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

let make_test_case guid_str prefix typeid_str =
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

let () =
  let open Alcotest in
  let empty =
    make_test_case "00000000-0000-0000-0000-000000000000" ""
      "00000000000000000000000000"
  in
  let one =
    make_test_case "00000000-0000-0000-0000-000000000001" ""
      "00000000000000000000000001"
  in
  let ten =
    make_test_case "00000000-0000-0000-0000-00000000000a" ""
      "0000000000000000000000000a"
  in

  let sixteen =
    make_test_case "00000000-0000-0000-0000-000000000010" ""
      "0000000000000000000000000g"
  in
  let thrirty_two =
    make_test_case "00000000-0000-0000-0000-000000000020" ""
      "00000000000000000000000010"
  in
  let max =
    make_test_case "ffffffff-ffff-ffff-ffff-ffffffffffff" ""
      "7zzzzzzzzzzzzzzzzzzzzzzzzz"
  in
  let test =
    make_test_case "01890a5d-ac96-774b-bcce-b302099a8057" "prefix"
      "prefix_01h455vb4pex5vsknk084sn02q"
  in

  let valid_alphabet =
    make_test_case "0110c853-1d09-52d8-d73e-1194e95b5f19" "prefix"
      "prefix_0123456789abcdefghjkmnpqrs"
  in

  run "Valid"
    [
      ( "Empty",
        [
          test_case "Parse" `Quick empty.parse;
          test_case "Decode" `Quick empty.encode;
          test_case "Check Prefix" `Quick empty.check_prefix;
        ] );
      ( "One",
        [
          test_case "Parse" `Quick one.parse;
          test_case "Decode" `Quick one.encode;
          test_case "Check Prefix" `Quick one.check_prefix;
        ] );
      ( "Ten",
        [
          test_case "Parse" `Quick ten.parse;
          test_case "Decode" `Quick ten.encode;
          test_case "Check Prefix" `Quick ten.check_prefix;
        ] );
      ( "Sixteen",
        [
          test_case "Parse" `Quick sixteen.parse;
          test_case "Decode" `Quick sixteen.encode;
          test_case "Check Prefix" `Quick sixteen.check_prefix;
        ] );
      ( "Thirty Two",
        [
          test_case "Parse" `Quick thrirty_two.parse;
          test_case "Decode" `Quick thrirty_two.encode;
          test_case "Check Prefix" `Quick thrirty_two.check_prefix;
        ] );
      ( "Max",
        [
          test_case "Parse" `Quick max.parse;
          test_case "Decode" `Quick max.encode;
          test_case "Check Prefix" `Quick max.check_prefix;
        ] );
      ( "Valid UuidV7",
        [
          test_case "Parse" `Quick test.parse;
          test_case "Decode" `Quick test.encode;
          test_case "Check Prefix" `Quick test.check_prefix;
        ] );
      ( "Valid Alphabet",
        [
          test_case "Parse" `Quick valid_alphabet.parse;
          test_case "Decode" `Quick valid_alphabet.encode;
          test_case "Check Prefix" `Quick valid_alphabet.check_prefix;
        ] );
    ]
