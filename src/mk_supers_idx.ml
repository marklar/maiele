open Util;;  open Printf
open ExtList

module TM = Tbl_mkr.Make(Idx_file.IntListData)

(*
  For any given lxm, results do NOT include self.
*)

let create_super_idx (dir:string) : unit =
  let tbl = TM.create dir "lex.supers" in
  let lexicon = Lexicon.open_tbl dir in
  let num_lxms = (Lexicon.length lexicon) + 1 in
    for id = 1 to Lexicon.length lexicon do
      let supers = 
	let lxm = Lexicon.get lexicon id in
	  List.rev (Lexicon.shortest_super_ids lexicon lxm (id+1) num_lxms)
      in TM.push tbl supers
    done;
    TM.close tbl

let _ =
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith (sprintf "usage: %s <dir_name>" Sys.argv.(0))
  in create_super_idx dir_name
