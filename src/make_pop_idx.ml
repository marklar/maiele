(*
 * Must be run AFTER pop ranks are 'fixed' (normalized).
 * 
 *   1. sort ids by (associated) pop.
 *   2. insert ids into pop:id index, in order.
 * 
 *)

open Printf

let dir_name =
  try Sys.argv.(1)
  with _ -> failwith (sprintf "usage: %s <dir_name>" Sys.argv.(0))

let val_idx_assocs (vals:Int_ary.t) : (int * int) array =
  Array.init (Int_ary.length vals) (fun i -> (Int_ary.get vals i, (i+1)) )

let ids_sorted_by_pop () : int array =
  let pop_id_pairs = val_idx_assocs (Int_ary.from_file dir_name "res.pop.data") in
    Array.fast_sort compare pop_id_pairs;
    Array.map snd pop_id_pairs

module IF = Idx_file.IntArray

let _ =
  IF.write_all dir_name "res.pop.idx.data" (ids_sorted_by_pop())
