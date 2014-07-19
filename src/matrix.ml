(**
   For any lexeme, stores its listings for:
     - result IDs
     - pop ranks
*)

open Printf;;  open Util

module IAT = Int_ary_tbl

type t =
    { ids  : IAT.t
    ; pops : IAT.t
    }

let open_tbl (dir:string) : t =
  let tbl s = IAT.open_tbl dir (sprintf "mtx.0.%s" s) in
    { ids  = tbl "ids"
    ; pops = tbl "pops"
    }

let close t : unit =
  IAT.close t.ids;
  IAT.close t.pops
  
let ids  t (lexeme_id:int) =
  IAT.get_exn t.ids  lexeme_id

let pops t (lexeme_id:int) =
  IAT.get_exn t.pops lexeme_id
