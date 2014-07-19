(*
 * Read in SORTED file with lines:
 *     <product_code>  \t  <glu_id>
 * 
 * Insert product_codes into Str_tbl.
 * Make 
 * 
 *)

open ExtList;; open Pcre;; open Printf;; open Util

(*
  Make files:
     - Int: for making map from product_code_id -> glu_id
     - Str: for making str-tbl part of Lexicon (for storing product_codes)
*)
module Int = Idx_file.Int
module Str = Tbl_mkr.Make(Idx_file.StringData)

type line_info =
    { code     : string
    ; glu_id   : int
    }

let tab_re = regexp "\\t"
let make_line_info (line:string) : line_info =
  match split line ~rex:tab_re with
    | [code; id] ->
	{ code     = code
	; glu_id   = int_of_string id
	}
    | _ -> failwith $ sprintf "Wrong format for line: %s" line

let do_all_lines (in_file_name:string) (lexicon_mkr:Str.t)
    (code_2_glu_mkr:Int.t) : unit =
  let in_chan = open_in in_file_name in
    try
      while true do
	let li = make_line_info (input_line in_chan) in
	  Str.push lexicon_mkr    li.code;
	  Int.push code_2_glu_mkr li.glu_id
      done
    with End_of_file ->
      close_in in_chan
       
let _ =
  let (dir_name, code_file_name) =
    try
      (Sys.argv.(1), Sys.argv.(2))
    with _ ->
      failwith $ sprintf "usage: %s <dir_name> <file_name>" Sys.argv.(0)
  in
  let lexicon_mkr    = Str.create dir_name "code.lex"
  and code_2_glu_mkr = Int.create dir_name "code:glu"
  in 
    do_all_lines
      (Filename.concat dir_name code_file_name)
      lexicon_mkr
      code_2_glu_mkr;
    Str.close lexicon_mkr;
    Int.close code_2_glu_mkr
