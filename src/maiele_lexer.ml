open Pcre;; open Util;; open ExtList
open Lexer_util

(* type synonyms: easier to see what each fun is for. *)
type text = string  (* could be any of the others *)
type lexeme = string

let comma_after_non_digits_re = regexp "([^\\d]),"
let comma_before_non_digits_re = regexp ",([^\\d])"
let space_one_sub = subst " $1"
let one_space_sub = subst "$1 "
let rm_commas_not_between_digits : text -> text =
  replace ~rex: comma_before_non_digits_re ~itempl: space_one_sub |>
  replace ~rex: comma_after_non_digits_re ~itempl: one_space_sub

(*
In queries, a single '-' immediately preceeding a lexeme
means binary "NOT". That's why we don't remove those here.
*)
let word_bound_regexps =
  List.map regexp [ "(-$|- |--|\\.\\.)"  (* NOT lxm-initial hyphens *)
    ; "\\.\\.\\." (* ellipsis *)
    ; "[!\"\\(\\):;\\[\\]\\{\\}\\\\/\\|\\^<>=]"
    ]
let replace_non_space_word_bounds_w_spaces (text: text) : text =
  if String.length text = 1 then
    text
  else
    List.fold_left replace_w_space text word_bound_regexps

(*
Wrap payload in data ctor.
Then pattern match in funs.
Makes it clearer how funs relate to each other.

For both cleaning and lexing.
*)
type lexable = Doc of string | Query of string

let dash_prefix_re = regexp "(^-|\\s+-)"
let clean (lexable: lexable) : string =
  let str = match lexable with
    | Doc s -> replace_w_space s dash_prefix_re
    | Query s -> s
  (* must hyphenate BEFORE lowercasing due to camel-casing *)
  in (compact_whitespace |> String.lowercase |>
      hyphenate_number_word_bounds |> hyphenate_roman_numeral_word_bounds |>
      hyphenate_camelcases |> Diacritics.fold |>
      replace_non_space_word_bounds_w_spaces |>
      rm_commas_not_between_digits) str

(*
Get rid of init / final:
- comma
- period
- single quote
*)
let edge_punct_regexps =
  List.map regexp [ "^[,\\.']"  (* initial *)
    ; "[,\\.']$"  (* final *)
    ]
let strip_edge_punct (lxm: lexeme) : lexeme =
  List.fold_left elide lxm edge_punct_regexps

(*** public ***)

(*
Contains hyphen? Include its "split" parts.
Else, simply repeated (which is why we uniq).
*)
let hyphen_re = regexp "-"
let lexemes_with_hyphen_parts (lexemes: lexeme list) : lexeme list =
  let lexeme_lists =
    List.map
      (fun s -> s :: (split s ~rex: hyphen_re))
      lexemes
  in (rm_blanks |> uniq |> List.flatten) lexeme_lists

(*
Split into 'words'.
For both indexing and quering.
*)
let lex_str (cleaned_text: text) : lexeme list =
  let strs = split cleaned_text in
    rm_blanks (List.map strip_edge_punct strs)

let lex (lexable: lexable) : lexeme list =
  match lexable with
  | Doc _ ->
      let lexemes = lex_str (clean lexable) in
        lexemes_with_hyphen_parts lexemes
  | Query s ->
      let q' = Query (drop_init_article s) in
        (rm_prefix_substrs |> lex_str |> clean) q'

(*
For querying: Embolden.make_regexp()
For indexing: Doc_lexer.all_uniq_variants()
*)
let lex_with_variants : lexable -> lexeme list =
  List.flatten |> List.map Variant.uniq_variants |> lex
