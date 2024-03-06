(** Abstract type representing a TypeId. *)
type t

(** Construct a Typeid.t from a string. First parameter is the "prefix" *)
val make: string -> t

(** Convert a Typeid.t to a string. *)
val to_string: t -> string