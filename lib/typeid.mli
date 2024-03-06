(** Abstract type representing a TypeId. *)
type t

(** Construct a Typeid.t from a string. First parameter is the "prefix" *)
val make: string -> t

(** Convert a Typeid.t to a string. *)
val to_string: t -> string

(** Try to parse a string. *)
val of_string_option: string -> t option

val get_uuid : t -> Uuidv7.t
val get_prefix: t -> string
val get_suffix: t -> string

module Uuidv7 = Uuidv7
