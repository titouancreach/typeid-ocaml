let foobar () = Alcotest.(check (option string)) "foo" (Some "foo") (Some "foo")

let () =
  let open Alcotest in
  run "TypeId" [ ("Valid Tests case ", [ test_case "foo bar" `Quick foobar ]) ]
