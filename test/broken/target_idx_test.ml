
open Printf

let _ = 
  let target_id = 7947713 and
      domain = Searcher.domain_of_name "games"
  in
    print_endline (Domain.results_for_target domain target_id)

(*
  let lex_ids:Int_ary.t =
      Result_tbl.lexes_from_target_id (Domain.result_tbl domain) target_id
  in
    print_endline (Int_ary.to_json_ary lex_ids)
*)
