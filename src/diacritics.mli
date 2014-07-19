
val for_reg : char -> char list
(**
   for_reg non_diacritic_ch
       'n' -> ['n~']
       'e' -> ['e''; 'e^'; 'e`'] (etc.)
   Used for emboldening.
*)

val fold : string -> string
(**
   fold str_with_diacritics
       "don~a" -> "dona"
*)
