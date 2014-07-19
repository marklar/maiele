type t =
  | And  of (t * t)
  | Or   of (t * t)
  | Not  of t
  | Leaf of string
  | Empty_node

(** creating *)
val of_string : string -> t

(** using *)
val show : t -> string

val sans_matches : t -> (string -> bool) -> string list * t
