type t

val open_tbl : string -> string -> t
(** Directory name, file root. *)

val close : t -> unit

val length : t -> int

val get_exn : t -> int -> string
(** Indexes start at 1. *)

val iter  : (string -> 'a)        -> t -> unit
val iteri : (int -> string -> 'a) -> t -> unit
