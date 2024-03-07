let prefix_uppercase () =
  Alcotest.(check (option string))
    "The prefix should be lowercase with no uppercase letters" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "PREFIX_00000000000000000000000000"))

let prefix_numeric () =
  Alcotest.(check (option string))
    "The prefix should be lowercase with no numerics" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "12345_00000000000000000000000000"))

let prefix_period () =
  Alcotest.(check (option string))
    "The prefix can't have symbols, it needs to be alphabetic" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "pre.fix_00000000000000000000000000"))

let prefix_underscore () =
  Alcotest.(check (option string))
    "The prefix can't have symbols, it needs to be alphabetic" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "pre_fix_00000000000000000000000000"))

let prefix_ascii () =
  Alcotest.(check (option string))
    "The prefix can only have ascii letters" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "préfix_00000000000000000000000000"))

let prefix_space () =
  Alcotest.(check (option string))
    "The prefix can't have any spaces" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option " prefix_00000000000000000000000000"))

let prefix_length () =
  Alcotest.(check (option string))
    "The prefix can't be 64 characters, it needs to be 63 characters or less"
    None
    (Option.map Typeid.to_string
       (Typeid.of_string_option
          "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl_00000000000000000000000000"))

let empty_prefix () =
  Alcotest.(check (option string))
    "If the prefix is empty, the separator should not be there" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "_00000000000000000000000000"))

let only_separator () =
  Alcotest.(check (option string))
    "A separator by itself should not be treated as the empty string" None
    (Option.map Typeid.to_string (Typeid.of_string_option "_"))

let suffix_too_short () =
  Alcotest.(check (option string))
    "The suffix can't be 25 characters, it needs to be exactly 26 characters"
    None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_0000000000000000000000000"))

let suffix_too_long () =
  Alcotest.(check (option string))
    "The suffix can't be 27 characters, it needs to be exactly 26 characters"
    None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_000000000000000000000000000"))

let suffix_space () =
  Alcotest.(check (option string))
    "The suffix can't have any spaces" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_1234567890123456789012345 "))

let suffix_uppercase () =
  Alcotest.(check (option string))
    "The suffix should be lowercase with no uppercase letters" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_0123456789ABCDEFGHJKMNPQRS"))

let suffix_hyphens () =
  Alcotest.(check (option string))
    "The suffix should be lowercase with no uppercase letters" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_123456789-123456789-123456"))

let suffix_wrong_alphabet () =
  Alcotest.(check (option string))
    "The suffix should only have letters from the spec's alphabet" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_ooooooiiiiiiuuuuuuulllllll"))

let suffix_ambiguous_crockford () =
  Alcotest.(check (option string))
    "The suffix should not have any ambiguous characters from the crockford \
     encoding"
    None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_i23456789ol23456789oi23456"))

let suffix_hyphens_crockford () =
  Alcotest.(check (option string))
    "The suffix can't ignore hyphens as in the crockford encoding" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_123456789-0123456789-0123456"))

let suffix_overflow () =
  Alcotest.(check (option string))
    "The suffix should encode at most 128-bits" None
    (Option.map Typeid.to_string
       (Typeid.of_string_option "prefix_8zzzzzzzzzzzzzzzzzzzzzzzzz"))
