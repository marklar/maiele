(* take one big 'ids' postings file, split into by-value-range ids postings *)
open Util;;  open Printf

(* cfg: begin *)
let dir_name =
  try
    Sys.argv.(1)
  with _ ->
    failwith $ sprintf "usage: %s <dir_name> [<num_partitions>]" Sys.argv.(0)

let num_partials =
  try int_of_string Sys.argv.(2)
  with _ -> 10
(* cfg: end *)

let max_lxm_id =
  let lexicon = Lexicon.open_tbl dir_name "lex" in
    Lexicon.length lexicon

let big_tbl = Int_ary_tbl.open_tbl dir_name "mtx.ids"

module Tbl = Tbl_mkr.Make(Idx_file.IntAryData)

let build_one_partial (num:int) (lo_val:int) (hi_val:int) : unit =
  (* create table *)
  let ids = Tbl.create dir_name (sprintf "mtx.%d.%s" num "ids") in
    (* fetch subsets of data (from lo_val -> hi_val) from big_tbl.
       push data into table. *)
    for lxm_id = 1 to max_lxm_id do
      let vs =
	Ord_int_ary.values_in (Int_ary_tbl.get_exn big_tbl lxm_id) lo_val hi_val
      in Tbl.push ids vs
    done;
    (* close table. *)
    Tbl.close ids

let build_all_partials () : unit =
  let num_per_partial =
    let max_id = Int_ary_tbl.max_val big_tbl in
      (max_id / num_partials) + 1
  in
    for i = 0 to num_partials-1 do
      let lo_val = i*num_per_partial + 1 and
	  hi_val = (i+1) * num_per_partial
      in build_one_partial i lo_val hi_val
    done

let _ =
  build_all_partials ()
