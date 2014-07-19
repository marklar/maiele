module Int = Idx_file.Int
module Ints = Tbl_mkr.Make(Idx_file.IntListData)
module Strs = Tbl_mkr.Make(Idx_file.StringData)

type t =
    { pop_col         : Int.t
    ; sort_val_col    : Int.t   (* the non-normalized sort "length". *)
    ; target_ids_col  : Ints.t
    ; text_col        : Strs.t
    ; title_only_col  : Strs.t
    }

let create (dir:string) (prefix:string) : t =
  let f s = prefix ^ "." ^ s in
    { pop_col        = Int.create  dir (f "pop.data")
    ; sort_val_col   = Int.create  dir (f "sort_val.data")
    ; target_ids_col = Ints.create dir (f "target_ids")
    ; text_col       = Strs.create dir (f "text")
    ; title_only_col = Strs.create dir (f "title_only")
    }

let push t (pop:int) (sort_val:int) (target_ids:int list)
    (text:string) (title_only:string option) : unit =
  Int.push  t.pop_col        pop;
  Int.push  t.sort_val_col   sort_val;
  Ints.push t.target_ids_col target_ids;
  Strs.push t.text_col       text;
  Strs.push t.title_only_col (match title_only with
				  | None -> ""
				  | Some s -> s)

let close t : unit =
  Int.close  t.pop_col;
  Int.close  t.sort_val_col;
  Ints.close t.target_ids_col;
  Strs.close t.text_col;
  Strs.close t.title_only_col
