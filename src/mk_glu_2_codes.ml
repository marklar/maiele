(*
  Then add glu_2_code Int_ary_tbl.
  How does one build the pop.idx and target_ids.idx tables?
  -- make_target_idx.ml --
*)

open ExtList;; open Pcre;; open Printf;; open Util

module Tbl = Tbl_mkr.Make(Idx_file.IntListData)

(*
  iter thru code:glu,
  creating hash: glu_id -> code list
*)
let hash_from_tbl (ia:Int_ary.t) : (int, int) Hashtbl.t =
  let h = Hashtbl.create (Int_ary.length ia) in
    Int_ary.iteri
      (fun c g -> Hashtbl.add h g (c+1))
      ia;
    h

let min_and_max_glu_ids (dir_name:string) : (int * int) =
  let in_chan = open_in (Filename.concat dir_name "min_max_glu_id")
  in
    let min_glu_id = int_of_string (input_line in_chan)
    and max_glu_id = int_of_string (input_line in_chan)
    in
      (min_glu_id, max_glu_id)

let create_table (dir_name:string) (hash:(int, int) Hashtbl.t)
    (min_glu_id:int) (max_glu_id:int) : unit =
  let mkr = Tbl.create dir_name "glu:codes" in
    (* store min [1st glu_id] at pos 1. *)
    Tbl.push mkr [min_glu_id];
    (* make one entry per glu_id, regardless of whether code_ids.empty? *)
    for glu_id = min_glu_id to max_glu_id do
      let code_ids = List.sort ~cmp:compare (Hashtbl.find_all hash glu_id) in
	Tbl.push mkr code_ids
    done;
    Tbl.close mkr

let _ =
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith $ sprintf "usage: %s <dir_name>" Sys.argv.(0)
  in
  let g2c_hash = hash_from_tbl (Int_ary.from_file dir_name "code:glu")
  and (min_glu_id, max_glu_id) = min_and_max_glu_ids dir_name in
    create_table dir_name g2c_hash min_glu_id max_glu_id
