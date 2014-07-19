open Util;;  open Printf

module Tbl = Tbl_mkr.Make(Idx_file.IntAryData)

let get_in_tbls (dir:string) : Int_ary_tbl.t list =
  let rec loop acc n =
    try loop (Int_ary_tbl.open_tbl dir (sprintf "mtx.%d.ids" n) :: acc) (n+1)
    with _ -> acc
  in List.rev (loop [] 0)

let _ = 
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith (sprintf "usage: %s <dir_name>" Sys.argv.(0))
  in
  let in_tbls = get_in_tbls dir_name and
      lexicon = Lexicon.open_tbl dir_name "lex"
  in
    Tbl.with_tbl dir_name "cat.mtx.ids"
      (fun out_tbl ->
	 (* for each lexeme... *)
	 for lxm_id = 1 to Lexicon.length lexicon do
	   (* create an Int_ary by concat-ing arys from all partitions... *)
	   let ia =
	     Int_ary.concat
	       (List.map
		  (fun partition -> Int_ary_tbl.get_exn partition lxm_id)
		  in_tbls)
	   in
	     (* ...and push onto out_tbl *)
	     Tbl.push out_tbl ia
	 done )
