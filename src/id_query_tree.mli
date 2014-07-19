type t

val next_val_fun : t -> (int -> Int_ary.t) -> (unit -> int)

val from_str_tree : Str_query_tree.t -> Lexicon.t -> t

val show : t -> string
