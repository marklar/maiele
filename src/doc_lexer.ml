open Pcre
open Util
open Lexer_util

type lexeme = string

(*
  dump_postings
*)
let all_uniq_variants (d:string) : lexeme list =
  rm_prefix_substrs $ Maiele_lexer.lex_with_variants (Maiele_lexer.Doc d)


(*
  For determining sort order.
    - Write_str_file
    - Create_faux
*)
let non_word_chs_re = regexp "\\W"
let space_tm = subst " "
let canonicalize_str : string -> string =
  let drop_non_word_chs = replace ~rex:non_word_chs_re ~itempl:space_tm in
    compact_whitespace |> drop_non_word_chs |> Diacritics.fold |>
	String.lowercase |> drop_init_article

let sort_len_str (s:string) : string =
  let str4 = zero_pad 4 |> string_of_int in
    str4 (List.length $ Maiele_lexer.lex (Maiele_lexer.Doc s)) ^
      str4 (String.length s)
