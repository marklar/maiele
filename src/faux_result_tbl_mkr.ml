open Idx_file

module Str_tbl  = Tbl_mkr.Make(StringData)
module Ints_tbl = Tbl_mkr.Make(IntAryData)

type t = { pop_col        : Int.t
	 ; text_col       : Str_tbl.t
	 ; target_ids_col : Ints_tbl.t
	 ; is_faux_col    : Bool.t
	 }

let create (dir_name:string) (prefix:string) : t =
    { pop_col        = Int.create      dir_name (prefix ^ ".pop.data")
    ; text_col       = Str_tbl.create  dir_name (prefix ^ ".text")
    ; target_ids_col = Ints_tbl.create dir_name (prefix ^ ".target_ids")
    ; is_faux_col    = Bool.create     dir_name (prefix ^ ".is_faux.data")
    }

let push t (pop:int) (target_ids:Int_ary.t) (text:string) (is_faux:bool) : unit =
  Int.push      t.pop_col        pop;
  Str_tbl.push  t.text_col       text;
  Ints_tbl.push t.target_ids_col target_ids;
  Bool.push     t.is_faux_col    is_faux

let close t : unit =
  Int.close      t.pop_col;
  Bool.close     t.is_faux_col;
  Str_tbl.close  t.text_col;
  Ints_tbl.close t.target_ids_col
