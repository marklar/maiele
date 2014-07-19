open Printf
open Util

module Ints = Tbl_mkr.Make(Idx_file.IntArrayData)

(*
  Given an Int_ary.t of result-ID postings,
  create a sorted int array of pop ranks.
*)
let pops_ary (ids:Int_ary.t) (res_tbl:Result_tbl.t) : int array =
  let pops = Array.make (Int_ary.length ids) 0 in
    Int_ary.iteri
      (fun idx res_id -> pops.(idx) <- Result_tbl.pop res_tbl res_id)
      ids;
    Array.fast_sort compare pops;
    pops
  
(*
  For each lexeme's ID postings:
    - create corresponding pops postings, and
    - push them into pops tbl.
*)
let do_it (dir:string) : unit =
  let res_tbl = Result_tbl.open_tbl dir and
      ids_tbl = Int_ary_tbl.open_tbl dir "mtx.0.ids"
  in
    Ints.with_tbl dir "mtx.0.pops"
      (fun tbl_mkr ->
	 Int_ary_tbl.iter
	   (fun ids -> Ints.push tbl_mkr $ pops_ary ids res_tbl)
	   ids_tbl )


let _ =
  let dir =
    try Sys.argv.(1)
    with _ -> failwith $ sprintf "Usage: %s <dir_name>" Sys.argv.(0)
  in do_it dir
    
