open ExtList;; open Pcre;; open Printf;; open Util

module LI = Line_info
module RTM = Result_tbl_mkr

(* line_infos: in REV sort order. *)
let write_result (calc_pop_rank:LI.pop_rank_fun) (rtm:RTM.t)
    (line_infos:LI.t list) : unit =
  let most_pop = LI.most_popular_exn line_infos in
    RTM.push rtm
      (calc_pop_rank most_pop line_infos)
      (LI.sort_val   most_pop)
      (LI.target_ids line_infos)
      (LI.text       most_pop)
      (LI.title_only most_pop)

let do_all_lines (rtm:RTM.t) (calc_pop_rank:LI.pop_rank_fun)
    (file_name:string) : unit =
  let in_chan = open_in file_name in
  let rec loop line_infos id =
    let line = 
      try Some (input_line in_chan)
      with _ -> None
    in match line with 
      | None -> write_result calc_pop_rank rtm line_infos 
      | Some ln -> 
	  let li = LI.from_line ln in 
	    match line_infos with 
	      | [] ->
		  (* first line of file. *)
		  loop [li] (id+1)
	      | hd :: _ when LI.are_same_result hd li ->
		  (* part of prev group. *) 
		  loop (li::line_infos) id
	      | _  ->
		  (* new group:  1. write prev group.  2. create new one. *)
		  write_result calc_pop_rank rtm line_infos;
		  loop [li] (id+1)
  in loop [] 0
       
       
let _ =
  let (dir_name, domain_name) =
    try
      (Sys.argv.(1), Sys.argv.(2))
    with _ ->
      failwith $ sprintf "usage: %s <dir_name> <domain_name>" Sys.argv.(0)
  in
  let rtm = RTM.create dir_name "res" in
    do_all_lines rtm
      (LI.calc_pop_rank_fun domain_name)
      (Filename.concat dir_name "results.txt.sort");
    RTM.close rtm
