(** Underlying type for Uuidv7 *)
type t

(** Create a new random Uuidv7. *)
val make: unit -> t

(** Convert a Typeid.t to a new uint128 (from Stdint). *)
val to_uint128: t -> Stdint.Uint128.t