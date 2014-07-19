type t = { low_idx : int
	 ; tbl     : Int_ary_tbl.t
	 }

let open_tbl (dir_name:string) (name:string) : t =
  let tbl = Int_ary_tbl.open_tbl dir_name name in
    { low_idx = Int_ary.hd (Int_ary_tbl.get_exn tbl 1)
    ; tbl     = tbl
    }

let close t : unit = Int_ary_tbl.close t.tbl

(* at idx 1, we store low_idx. *)
let first_used_idx = 2

let get t (id:int) : Int_ary.t =
  let real_idx = (id - t.low_idx + first_used_idx) in
    Int_ary_tbl.get_exn t.tbl real_idx
