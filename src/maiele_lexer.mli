(**
   Splits a string into "lexemes" (i.e. word-like strings of characters).

   (* -> string list *)
   Lexer.lex (Doc "some string to split")
*)

type lexeme = string
type lexable = Doc of string | Query of string

val lex : lexable -> lexeme list
val lex_with_variants : lexable -> lexeme list
