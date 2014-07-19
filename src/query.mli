type t

val doc_only : t -> Str_query_tree.t
val code_only_opt : t -> string option

val from_string : bool -> string -> t
val show : t -> string
