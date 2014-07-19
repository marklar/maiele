open Printf;;  open Util

module Iat = Int_ary_tbl
module Tbl = Tbl_mkr.Make(Idx_file.IntListData)

(* iter thru ts, creating hash: val -> ids *)
let hash_from_tbl (vals_tbl:Iat.t) (size:int) : (int, int) Hashtbl.t =
  let hash = Hashtbl.create size in
    Iat.iteri
      (fun id vals ->
	 Int_ary.iter (fun v -> Hashtbl.add hash v id) vals )
      vals_tbl;
    hash

let create_table (dir_name:string) (tbl_name:string)
    (hash:(int, int) Hashtbl.t) (min:int) (max:int) : unit =
  let mkr = Tbl.create dir_name (tbl_name ^ ".idx") in
    (* store min [1st val] at pos 1. *)
    Tbl.push mkr [min];
    (* make one entry per val, regardless of whether ids.empty? *)
    for v = min to max do
      let ids = List.sort compare (Hashtbl.find_all hash v) in
	Tbl.push mkr ids
    done;
    Tbl.close mkr

let _ =
  let (dir_name, tbl_name) =
    try (Sys.argv.(1), Sys.argv.(2))
    with _ -> failwith (sprintf "usage: %s <dir_name> <tbl_name>" Sys.argv.(0))
  in
  let tbl = Iat.open_tbl dir_name tbl_name in
  let (min, max) = (Iat.min_val tbl, Iat.max_val tbl) in
  let hash = hash_from_tbl tbl (max - min + 1) in
    create_table dir_name tbl_name hash min max
