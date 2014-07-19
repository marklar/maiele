
type t =
  { in_stock : Bool_ary.t
  ; tag_ids  : Int_ary_tbl.t
  }

let open_tbl (dir:string) : t =
  { in_stock = Bool_ary.from_file ~shared:true dir "glu.in_stock.data"
  ; tag_ids  = Int_ary_tbl.open_tbl dir "glu.tag_ids"
  }
  
let close t : unit =
  Bool_ary.close t.in_stock;
  Int_ary_tbl.close t.tag_ids

let tag_ids t (id:int) : Int_ary.t =
  Int_ary_tbl.get_exn t.tag_ids id

let in_stock t (id:int) : bool =
  Bool_ary.get t.in_stock id

let set_in_stock t (id:int) (in_stock:bool) : unit =
  Bool_ary.set t.in_stock id in_stock