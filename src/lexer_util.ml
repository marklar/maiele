open Pcre;; open ExtList

type text = string

(*  See test suite for examples of behavior.  *)

let replace_w_space (str: string) (rex: Pcre.regexp) =
  replace str ~rex ~itempl: Util.space_sub

let elide (str: string) (rex: Pcre.regexp) =
  replace str ~rex ~itempl: Util.empty_sub

let lower_upper_re = regexp "([a-z])([A-Z])"
let insert_hyphen_sub = subst "$1-$2"
(*
"fooBarBAZ" => "foo-Bar-BAZ"
*)
let hyphenate_camelcases (s: string) : string =
  replace s ~rex: lower_upper_re ~itempl: insert_hyphen_sub

let char_digit_re = regexp "([a-zA-Z])(\d)"
(*
"Galaxy S3" => "Galaxy S-3"
*)
let hyphenate_number_word_bounds (s: string) : string =
  replace s ~rex: char_digit_re ~itempl: insert_hyphen_sub

let roman_numeral_re = regexp "([^i\s\d\W])(iv|i{2,3})(\s|$)"
(*
"galaxy sii" => "galaxy s-ii"
*)
let insert_hyphen_sub_with_word_end = subst "$1-$2$3"
let hyphenate_roman_numeral_word_bounds (s: string) : string =
  replace s ~rex: roman_numeral_re ~itempl: insert_hyphen_sub_with_word_end


(*
For query strings.
(And for titles pulled from DB in indexer.)
*)
let init_the_re = regexp "^[Tt]he "
let init_a_re = regexp "^[Aa] "
let drop_init_article (s: string) : string =
  let sans_the = elide s init_the_re in
    if sans_the <> s then
      sans_the
    else
      elide s init_a_re

(*
Filters out all blank strings from list.
*)
let rm_blanks : string list -> string list = List.filter ((<>) "")

(*
[ "foo"; "foobar"; "bar" ] => [ "foobar"; "bar" ]
Filters out only those which are prefixes of another.

already reverse - sorted:
[ "fools"; "foolish"; "food"; "foobar"; "foo" ]
*)
let rm_prefix_substrs (ss: string list) : string list =
  let rec loop acc ss' = match ss' with
    | [] -> acc
    | hd :: tl -> match acc with
        | [] -> loop [hd] tl
        | s :: _ ->
        (* "Is the *previous* word a super of me?" *)
            if Util.superstr_p s hd then
              loop acc tl
            else
              loop (hd:: acc) tl
  in loop []
      (* reverse sort *)
      (List.sort ~cmp: (fun a b -> compare b a) ss)
