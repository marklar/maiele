type t

val from_file : ?shared:bool -> string -> string -> t
  (** dir_name file_name *)
val close  : t -> unit
val length : t -> int
val get    : t -> int -> bool
val set    : t -> int -> bool -> unit
