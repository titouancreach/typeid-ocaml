# OCaml TypeId

An OCaml, following the [specs](https://github.com/jetpack-io/typeid), implementation of [TypeId](https://github.com/jetpack-io/typeid). Also provide a implementation of [UUIDv7](https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-5.2)

Usage: 
```ocaml
let () =
  Random.self_init ();
  let id = Typeid.make "myprefix" in
  let id_str = Typeid.to_string id in
  Printf.printf "%s" id_str;
  match Typeid.from_string_option id_str with
    | Some x -> (* my typeid *)
    | None -> failWith "Can't parse typeid"

```

> [!NOTE]
> UuidV7 uses random bits so, you need to initialize the random pseudo generator yourself.



