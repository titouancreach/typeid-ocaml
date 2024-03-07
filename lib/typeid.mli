(** Abstract type representing a TypeId. *)
type t

(** Construct a Typeid.t from a string. First parameter is the "prefix" *)
val make: string -> t

(** Convert a Typeid.t to a string. *)
val to_string: t -> string

(** Try to parse a string. *)
val of_string_option: string -> t option

(** Get the uuid part of the TypeId. *)
val get_uuid : t -> Uuidv7.t

(** Get the prefix of the TypeId. *)
val get_prefix: t -> string

(** Get the suffix of the TypeId. *)
val get_suffix: t -> string

(** Create a t from prefix (string) and a uuid. *)
val of_guid: string -> Uuidv7.t -> t

module Uuidv7 = Uuidv7
