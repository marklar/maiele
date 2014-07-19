(*
  Using the Result_tbl, fetch:
    - text
    - result ID
  and output into flat file.
*)
open Util;;  open Printf

let sort_str (i:int) : string =
  zero_pad 9 (string_of_int i)

(* CONFIG *)
let max_substr_len = 4
(* CONFIG *)

(*--- two different versions of 'get_variants' functions ---*)

(* But only 34 out of 7+ MM do NOT begin with '978'. *)
let upc_substrings (s:string) : string list =
  let rec loop acc len =
    if len > 0 then
      loop (String.sub s 0 len :: acc) (len-1)
    else
      acc
  in s :: (loop [] max_substr_len)

let variants_and_conflations (s:string) : string list =
  let vs = Doc_lexer.all_uniq_variants s in
  let cs = List.flatten (List.map Conflator.conflations vs) in
    vs @ cs

(* each lexeme appears multiple times.  output 1 line per appearance. *)
let postings (dir_name:string) (get_variants:string -> string list) : unit =
  let dump_postings (id:int) (text:string) : unit =
    List.iter
      (fun lxm -> printf "%s\t%s\n" lxm (sort_str id))
      (get_variants text)
  in Str_tbl.iteri dump_postings (Str_tbl.open_tbl dir_name "res.text")

let _ =
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith "posting: dump_postings <dir_name>"
  and f =
    let is_upcs =
      try Sys.argv.(2) = "upcs"
      with _ -> false
    in
      if is_upcs then
	upc_substrings
      else
	variants_and_conflations
  in postings dir_name f
