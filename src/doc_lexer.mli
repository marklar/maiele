type lexeme = string

val all_uniq_variants : string -> lexeme list
  (** for dump_usages.
      for a string with multiple tokens,
      return a list of all different token variants from that string.
  *)

val canonicalize_str : string -> string
val sort_len_str : string -> string
