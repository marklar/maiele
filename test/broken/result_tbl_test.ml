open Printf

let _ =

(*
  let pop =
    try int_of_string Sys.argv.(1)
    with _ -> failwith (sprintf "usage: %s <pop_rank>" Sys.argv.(0)) in

  let rt = Result_tbl.open_tbl "games" in
  let id = Result_tbl.lex_from_pop rt pop in

    printf "pop: %d\n" pop;
    printf "id from pop: %d\n" id;
    printf "pop from id: %d\n" (Result_tbl.pop rt id);
    printf "%s\n\n" (Result_tbl.text rt id)
*)

  let rt = Result_tbl.open_tbl "games" in
    for id = 1 to Result_tbl.length rt do
      printf "%d :: %d :: %s\n" id (Result_tbl.pop rt id) (Result_tbl.text rt id);
      printf "  faux?: %d\n" (if Result_tbl.is_faux rt id then 1 else 0);
      Int_ary.iter (printf "%d\n") (Result_tbl.target_ids rt id);
      print_endline ""
    done
