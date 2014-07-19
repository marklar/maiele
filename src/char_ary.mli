(**
  Mmap a file containing Strings.

  (* Get string of the chars 10-15 from file. *)
  let strings = Char_ary.from_file dir_name file_name in
    Char_ary.unsafe_get_str strings 10 5
*)

type t

val from_file : ?shared:bool -> string -> string -> t
  (** dir_name file_name *)

val close : t -> unit

val length : t -> int

val unsafe_get_str : t -> int -> int -> string

(** not in use *)
val get_exn : t -> int -> char
val unsafe_get : t -> int -> char
val set_exn : t -> int -> char -> unit
val unsafe_set : t -> int -> char -> unit
