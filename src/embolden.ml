open Util;;  open Pcre;;  open Printf

(*
  Given a string, return one string per character,
  where each string contains that character plus any
  diacritic versions of same.
  >> "na" -> ["NN~nn~"; "AA'A`aa'a`"]
*)
let diacs_str_per_char (s:string) : string list =
  let rec loop acc idx =
    if idx < 0 then acc else
      let str_for_one_char =
	let case_strs =
	  let str =
	    try
	      string_from_chars (s.[idx] :: Diacritics.for_reg s.[idx])
	    with _ ->
	      String.sub s idx 1
	  in
	    if idx = 0 then
	      (* Uppercase-only regexp charset for char #1,
		 to match camelcase. *)
	      String.uppercase str
	    else
	      (* Both upper- and lowercase regexp charset
		 for all other chars in token.
		 Necessary for camelcase match, 
		 since we cannot use `CASELESS match. *)
	      (String.uppercase str) ^ (String.lowercase str)
	in
	  (* Regexp-quote each, in case any contain non-literal patterns. *)
	  "[" ^ quote case_strs ^ "]"
      in
	loop (str_for_one_char :: acc) (idx-1)
  in loop [] ((String.length s) - 1)
	       

(* For each lexeme, make a regexp pattern which matches:
   - each character or diacritic version of same
   - interspersed with optional certain punctuation (-',.)
*)
let pattern_for_str (lxm:string) : string =
  let diacs_strs = diacs_str_per_char lxm in
    String.concat "[\\-',\\.]?" diacs_strs

(*
  Conflations match only when COMPLETE (i.e. followed by:
     - whitespace
     - non-word char
     - EOL)
  Uses positive lookahead assertion.
*)
let pattern_for_conflation (conflation:string) : string =
  let p = pattern_for_str conflation in
    sprintf "%s(?=[\\s\\W])|%s$" p p

let pattern_for_1_lexeme (lxm:string) : string =
  let conflation_patterns =
    (* Use this version if not requiring that conflation be complete. *)
    (* List.map pattern_for_str (Conflator.conflations lxm) *)
    List.map pattern_for_conflation (Conflator.conflations lxm)
  in
    String.concat "|" (pattern_for_str lxm :: conflation_patterns)

(*
  Sort the query lexemes, longest first.
  (Could also do reverse-alpha, but String.length is O(1).)

  Why?  We attempt to match them in that order.  If we did shorter
  lexemes first, one of them might be a substring of a longer one, and
  we could no longer match the longer one because it would be
  interrupted with emboldening mark-up.
*)
let long_to_short (lxms:string list) : string list =
  let by_desc_length a b = compare (String.length b) (String.length a)
  in List.fast_sort by_desc_length (Util.uniq lxms)

(* Make one big regexp from list of per-lexeme patterns. *)
let patterns_for_all_lexemes (lxms:string list) : string list =
  List.map pattern_for_1_lexeme (long_to_short lxms)


let double_ch i = let c = char_of_int i in sprintf "%c%c" c c
let improbable_pre      = double_ch 254
let improbable_post     = double_ch 255
let improbable_bold_str = sprintf "$1%s$2%s" improbable_pre improbable_post
let improbable_bold_tm  = subst improbable_bold_str
let beg_real_re = regexp improbable_pre
let beg_real_tm = subst "<em>"
let end_real_re = regexp improbable_post
let end_real_tm = subst "</em>"

(*
  Create a pretty complicated list of regexps
  for emboldening query tokens.
*)
let replace_nothing_re = regexp "(\\s)()"
let make_regexps (query:string) : regexp list =
  (* Treat query as a Lexer.Doc (not Query),
     because for the purposes of making a Regexp to embolden results,
     we do NOT want to:
       - remove initial article,
       - leave in hyphen prefix (to mean 'NOT'),
       - remove prefix substrings
  *)
  match Maiele_lexer.lex_with_variants (Maiele_lexer.Doc query) with
    | []   -> [replace_nothing_re]
    | lxms ->
	let pats = patterns_for_all_lexemes lxms in
	let regulars =
	  (* match only:
	     - at beginning
	     - after whitespace
	     - after non-word character *)
	  List.map
	    (fun pat ->
	       regexp ~flags:[`CASELESS] (sprintf "(^|\\s|\\W)(%s)" pat))
	    pats
	and camelcases =
	  (* match only in the middle of a token:
	     - after '</em>' (from a 'regulars' match)
	     - after lowercase character *)
	  List.map
	    (fun pat ->
	       regexp (sprintf "(%s|[a-z])(%s)" improbable_post pat))
	    pats
	in regulars @ camelcases

(*
  Wrap each to-be-emboldened substring in "<em>" and "</em>".
    - $1: the space (or beg of line) which comes before the match.
    - $2: the match itself.
*)
let embolden (html:string) (regexps:Pcre.regexp list) : string =
  let sub_w_improbable s rex =
    replace s ~rex ~itempl:improbable_bold_tm
  and sub_beg s =
    replace s ~rex:beg_real_re ~itempl:beg_real_tm
  and sub_end s =
    replace s ~rex:end_real_re ~itempl:end_real_tm
  in
    sub_end (sub_beg (List.fold_left sub_w_improbable html regexps))
