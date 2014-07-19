
val make_regexps : string -> Pcre.regexp list
(**
   Given unmodified query string, make a list of regexps
   to highlight matching tokens in a result text.
   (See .ml-file comments for implementation details.)
*)

val embolden : string -> Pcre.regexp list -> string
