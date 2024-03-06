let () =
  Random.self_init ();
  let id = Typeid.make "my_prefix" in
  Printf.printf "My id: %s \n" (Typeid.to_string id)
