open Pcre;;  open Util;;  open ExtList
open Lexer_util

type lexeme = string

let only_alphanum_re = regexp "^[0-9a-zA-Z]*$"

(*
  The ORDER of these regexps matters.
  We apply them in this order.
  Each is used to elide char(s) from the lexeme.
*)
let single_variant_regexps =
  List.map regexp
    [ "^[^a-z0-9]|[^a-z0-9\\+\\&]$"  (* boundary punct *)
    ; "\\*"                          (* asterisk *)
    ; ","                            (* comma *)
    ; "'s$"                          (* final apostrophe *)
    ; "'"                            (* apostrophe *)
    ; "\\."                          (* period *)
    ; "-"                            (* hyphen *)
    ]

(*
  >>  ["spider-man's";  "identity"]    /'s$/
  =>  ["spider-man";    "identity"]
*)
let get_variants (lxms:lexeme list) (rex:Pcre.regexp) : lexeme list =
  List.map (fun s -> elide s rex) lxms

(*
  For indexing and querying.
    - Str_query_tree
    - all_uniq_variants()
  Expects: lexeme is lowercase.

  - Start with a single lexeme (e.g. "spider-man's").

  - Apply each regexp (above) in turn, perhaps finding a new variant,
    and if so, adding it to our accumulating list:
       at start:
           [ "spider-man's" ]
       after final apostrophe:
           [ "spider-man's"; "spider-man" ]
       after hyphen:
           [ "spider-man's"; "spider-man"; "spiderman's"; "spiderman" ]
*)
let uniq_variants (lxm:lexeme) : lexeme list =
  if pmatch lxm ~rex:only_alphanum_re
  then [lxm]
  else
    let variants =
      (*
	how to apply one regexp.
	(uniq each time, to avoid redundant work.)
      *)
      let add_uniq_variants lxms rex =
	uniq $ List.rev_append lxms (get_variants lxms rex)
      in
	(* apply each regexp in turn *)
	List.fold_left
	  add_uniq_variants
	  [lxm]
	  single_variant_regexps
    in
      (* there might be a single blank str *)
      rm_blanks variants
