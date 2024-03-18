(lang dune 3.12)

(name typeid)

(generate_opam_files false)

(source
 (github titouancreach/typeid-ocaml))

(authors "CREACH Titouan")

(maintainers "CREACH Titouan")

(license MIT)

(documentation https://github.com/titouancreach/typeid-ocaml)

(package
 (name typeid)
 (synopsis "Typeid and UUIDv7 implementation in OCaml")
 (description "TypeId and UUIDv7 implementation in OCaml")
 (depends
  (ocaml
   (>= 4.14))
  (alcotest :with-test)
  dune
  stdint
  batteries)
 (tags
  (uuidv7 typeid)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
