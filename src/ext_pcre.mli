
val replace_hexes : string -> string

val matches_from_strs : Pcre.regexp -> string list -> string array list

val matches_from_strs_enum : Pcre.regexp -> string Enum.t -> string array Enum.t
