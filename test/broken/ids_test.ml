(* 2008, Mark Wong-VanHaren, Glyde Corp. *)

let _ =
  let domain_name = Array.get Sys.argv 1
  and result_id   = int_of_string (Array.get Sys.argv 2)
  in
    print_endline (Int_ary.to_json_ary (Searcher.target_ids domain_name result_id))
