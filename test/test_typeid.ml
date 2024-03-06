let test_titouan () =
  Alcotest.(check string)
    "same string" "01890a5d-ac96-774b-bcce-b302099a8057"
    (Typeid.Uuidv7.to_string
       (Typeid.get_uuid
          (Option.get
             (Typeid.of_string_option "prefix_01h455vb4pex5vsknk084sn02q"))))

let () =
  let open Alcotest in
  run "Typeid" [ ("Decode Typeid", [ test_case "uuid1" `Quick test_titouan ]) ]
