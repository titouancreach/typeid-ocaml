let () =
  Random.self_init ();
  let id = Typeid.make "myprefix" in
  Printf.printf "My id: %s \n" (Typeid.to_string id)
