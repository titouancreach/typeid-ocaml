# OCaml TypeId

An OCaml, following the [specs](https://github.com/jetpack-io/typeid), implementation of [TypeId](https://github.com/jetpack-io/typeid). Also provide a implementation of [UUIDv7](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-5.2)

Usage: 
```ocaml
let () =
  Random.self_init ();
  let id = Typeid.make "my_prefix" in
  Printf.printf "My id: %s \n" (Typeid.to_string id)
```


