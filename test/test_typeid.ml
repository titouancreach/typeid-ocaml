let () =
  let open Alcotest in
  let empty =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-000000000000" ""
      "00000000000000000000000000"
  in
  let one =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-000000000001" ""
      "00000000000000000000000001"
  in
  let ten =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-00000000000a" ""
      "0000000000000000000000000a"
  in

  let sixteen =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-000000000010" ""
      "0000000000000000000000000g"
  in
  let thirty_two =
    Valid_test.make_valid_test_case "00000000-0000-0000-0000-000000000020" ""
      "00000000000000000000000010"
  in
  let max =
    Valid_test.make_valid_test_case "ffffffff-ffff-ffff-ffff-ffffffffffff" ""
      "7zzzzzzzzzzzzzzzzzzzzzzzzz"
  in
  let test =
    Valid_test.make_valid_test_case "01890a5d-ac96-774b-bcce-b302099a8057"
      "prefix" "prefix_01h455vb4pex5vsknk084sn02q"
  in

  let valid_alphabet =
    Valid_test.make_valid_test_case "0110c853-1d09-52d8-d73e-1194e95b5f19"
      "prefix" "prefix_0123456789abcdefghjkmnpqrs"
  in

  run "TypeId"
    [
      ( "Valid Tests case ",
        [
          test_case "Parse an empty TypeId" `Quick empty.parse;
          test_case "Decode an empty TypeId" `Quick empty.encode;
          test_case "Check an empty Prefix" `Quick empty.check_prefix;
          test_case "Parse TypeId 1" `Quick one.parse;
          test_case "Decode TypeId 1" `Quick one.encode;
          test_case "Check Prefix 1" `Quick one.check_prefix;
          test_case "Parse TypeId 10" `Quick ten.parse;
          test_case "Decode TypeId 10" `Quick ten.encode;
          test_case "Check Prefix TypeId 10" `Quick ten.check_prefix;
          test_case "Parse TypeId 16" `Quick sixteen.parse;
          test_case "Decode TypeId 16" `Quick sixteen.encode;
          test_case "Check Prefix TypeId 16" `Quick sixteen.check_prefix;
          test_case "Parse TypeId Max" `Quick max.parse;
          test_case "Decode TypeId max" `Quick max.encode;
          test_case "Check Prefix Max" `Quick max.check_prefix;
          test_case "Decode 32 TypeId" `Quick thirty_two.encode;
          test_case "Parse 32 TypeId" `Quick thirty_two.parse;
          test_case "Check Prefix TypeId 2" `Quick thirty_two.check_prefix;
          test_case "Parse TypeId 2" `Quick test.parse;
          test_case "Decode TypeId Valid" `Quick test.encode;
          test_case "Check Prefix TypeId Valid" `Quick test.check_prefix;
          test_case "Parse TypeId Alphabet" `Quick valid_alphabet.parse;
          test_case "Decode TypeId Alphabet" `Quick valid_alphabet.encode;
          test_case "Check Prefix TypeId Alphabet" `Quick
            valid_alphabet.check_prefix;
        ] );
      ( "Invalids",
        [
          test_case "With prefix uppercase" `Quick
            Invalid_tests.prefix_uppercase;
          test_case "With prefix numeric" `Quick Invalid_tests.prefix_numeric;
          test_case "With prefix period" `Quick Invalid_tests.prefix_period;
          test_case "With prefix underscore" `Quick
            Invalid_tests.prefix_underscore;
          test_case "With prefix non ascii" `Quick Invalid_tests.prefix_ascii;
          test_case "With prefix spaces" `Quick Invalid_tests.prefix_space;
          test_case "With prefix too long" `Quick Invalid_tests.prefix_length;
          test_case "With empty prefix" `Quick Invalid_tests.empty_prefix;
          test_case "With only separator" `Quick Invalid_tests.only_separator;
          test_case "With suffix too short" `Quick
            Invalid_tests.suffix_too_short;
          test_case "With suffix too long" `Quick Invalid_tests.suffix_too_long;
          test_case "With suffix with spaces" `Quick Invalid_tests.suffix_space;
          test_case "With suffix with uppercase" `Quick
            Invalid_tests.suffix_uppercase;
          test_case "With suffix with hyphens" `Quick
            Invalid_tests.suffix_hyphens;
          test_case "With suffix with wrong alphabet" `Quick
            Invalid_tests.suffix_wrong_alphabet;
          test_case "With suffix with ambiguous crockford" `Quick
            Invalid_tests.suffix_ambiguous_crockford;
          test_case "With suffix with hyphens crockford" `Quick
            Invalid_tests.suffix_hyphens_crockford;
          test_case "With suffix overflow" `Quick Invalid_tests.suffix_overflow;
        ] );
    ]
