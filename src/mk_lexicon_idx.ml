(*
  Last step in creating a Lexicon.

  For each char code (0-255),
  find the first lexeme (if any) in the
  existing Str_tbl which starts with it.
  Record that.
*)
open Util
open Printf

(* cfg: begin *)
let dir_name  = try Sys.argv.(1) with _ -> "."
let file_root = try Sys.argv.(2) with _ -> "lex"
(* cfg: end *)

(* read from lexicon; set array values. *)
let first_ids_from_str_tbl (str_tbl:Str_tbl.t) : int array =
  let ids = Array.make 256 0 in   (* 256: per head char *)
    Str_tbl.iteri
      ( fun i lxm ->
	  let hd = Char.code lxm.[0] in  (* no empty strs *)
	    if   ids.(hd) =  0           (* novel hd char *)
	    then ids.(hd) <- i )
      str_tbl;
    ids

(* read from array; write to index file. *)
let create_idx_file (first_ids:int array) : unit =
  Idx_file.IntArray.write_all
    dir_name
    (sprintf "%s.hd:id.data" file_root)
    first_ids

let _ = 
  let str_tbl = Str_tbl.open_tbl dir_name file_root in
    create_idx_file $ first_ids_from_str_tbl str_tbl
