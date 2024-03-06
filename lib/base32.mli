(** Encode a Uuidv7.t to a base 32 encoded string. *)
val encode: Uuidv7.t -> string

(** Decode a base 32 encoded string to a Uuidv7.t. *)
val decode: string -> Uuidv7.t