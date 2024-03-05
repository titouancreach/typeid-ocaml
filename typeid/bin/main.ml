let () =
  Random.self_init ();
  for i = 0 to 10 do
    let id = Typeid.make "cgps" in
    Printf.printf "Encoded: %s \n" (Typeid.to_string id)
  done
