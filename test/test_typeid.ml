let foobar () = Alcotest.(check (option string)) "foo" (Some "foo") (Some "foo")

let () =
  let open Alcotest in
  let empty =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-000000000000" ""
      "00000000000000000000000000"
  in
  run "TypeId" [ ("Valid Tests case ", [ test_case "foo bar" `Quick foobar ]) ]
