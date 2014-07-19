type bigrams = string list

(** don't need to be public.  good for testing. *)
val intersection_len : bigrams -> bigrams -> int
val union_len : bigrams -> bigrams -> int

(** actually public *)
val bigrams_for_lowercase_string : string -> bigrams
val bigrams : string -> bigrams
val cmp_bigrams : bigrams -> bigrams -> float
val cmp_strs : string -> string -> float
val cmp_bigrams_and_string : bigrams -> string -> float
