(*
 * Make:
 *   - Str_tbl (most of a Lexicon -- missing the head-char index)
 *   - Matrix  (for each lexeme: just result IDs)
 *)

(*
  Use functor (Tbl_mkr) to make "maker" modules:
     - Ints: for recording postings
     - Str:  for making part of Lexicon
*)
module Ints = Tbl_mkr.Make(Idx_file.IntArrayData)
module Str  = Tbl_mkr.Make(Idx_file.StringData)

(* a matrix maker *)
type t =
    { ids     : Ints.t
    ; lexicon : Str.t
    }

(*-- exported --*)

(* open each table *)
let create (dir_name:string) : t =
  { ids     = Ints.create dir_name "mtx.ids"
  ; lexicon = Str.create  dir_name "lex"
  }

(* close each table *)
let close t : unit =
  Ints.close t.ids;
  Str.close  t.lexicon

(* add one lexeme's data to tables *)
let push t (ids:int array) (lexeme:string) : unit =
  Ints.push t.ids     ids;
  Str.push  t.lexicon lexeme
