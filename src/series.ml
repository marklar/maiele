open Pcre;; open Util;; open Printf

(*
  The "series" tag data Muze (Rovi) provides us for games
  is really quite bad.  It's inconsistent, redundant... just horrible.
  But we want to use it.  We want people to be able to search
  for game series.

  So, we do quite a lot of work here to clean up the "series" tag
  data before putting it into the Browse results.

  The function fix() is used (in Write_str_file) to take a series
  name (e.g. "For Dummies") and "fix" it so that it conforms to our standards.
*)

let first_match_tm = subst "$1"

let surrounding_parens_re = regexp "^\\((.*)\\)$"
let rm_surrounding_parens (s:string) : string =
  replace s ~rex:surrounding_parens_re ~itempl:first_match_tm

let surrounding_quotes_re = regexp "^\"(.*)\"$"
let rm_surrounding_quotes (s:string) : string =
  replace s ~rex:surrounding_quotes_re ~itempl:first_match_tm

let rm_surrounding_nonsense = rm_surrounding_quotes |> rm_surrounding_parens

let bogus_quote_re = regexp "\"\""
let single_tm = subst "\""
let rm_superfluous_quotes (s:string) : string =
  replace s ~rex:bogus_quote_re ~itempl:single_tm

let first_two_matches_tm = subst "$1$2"
let double_series_re =
  regexp ~flags:[`CASELESS] "^(.*\\Wseries)\\W*series(\\W*)$"
let rm_repetition_of_series (s:string) : string =
  Util.compact_whitespace
    (replace s ~rex:double_series_re ~itempl:first_two_matches_tm)

let ends_in_series_re = regexp ~flags:[`CASELESS] "^(.*)\\Wseries$"
let rm_final_series = replace ~rex:ends_in_series_re ~itempl:first_match_tm

let contains_series_re = regexp ~flags:[`CASELESS] "(^|\\W)series(\\W|$)"
let maybe_reappend_final_series (name:string) : string =
  if pmatch name ~rex:contains_series_re then
    name
  else
    sprintf "%s series" name

(*
  split into words [using WHITESPACE].  capitalize each word.
  FIXME: Don't use only whitespace.
*)
let unshout (s:string) : string =
  if String.uppercase s = s then
    String.concat " " $
      List.map String.capitalize (split (String.lowercase s))
  else
    s

let empty_tm = subst ""
let rm_final_vert_name (domain:string) (name:string) : string =
  let (sing, plur) = match domain with
    | "games" -> ("game", "games")
    | s -> failwith (sprintf "Not expecting series for domain: %s\n" s) in
  let rex = regexp ~flags:[`CASELESS] (sprintf "\\s(%s|%s)\\s*$" sing plur) in
    replace name ~rex ~itempl:empty_tm

(*
  The only real public value.
  The rest are exposed merely for the benefit of unit testing.
*)
let fix (name:string) (domain:string) : string =
  (maybe_reappend_final_series |> unshout |> rm_final_vert_name domain |>
       rm_surrounding_nonsense |> rm_final_series |> rm_surrounding_nonsense |>
	   rm_superfluous_quotes |> rm_repetition_of_series |> Entities.down) name
