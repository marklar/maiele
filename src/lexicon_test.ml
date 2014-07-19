open Printf

let _ =
  let lexicon =
    try Lexicon.open_tbl Sys.argv.(1)
    with _ -> failwith (sprintf "Usage: %s <dir_name>" Sys.argv.(0))
  and matrix = Matrix.open_tbl Sys.argv.(1) 0
  in
    (* i: lexeme ID *)
    for i = 1 to Lexicon.length lexicon do
      let ids = Matrix.ids matrix i in
      let limit = min 2000 (Int_ary.length ids) in
	if not (Int_ary.is_ordered ids) then begin
	  printf "%d : %s\n" i (Lexicon.get lexicon i);
	  printf "  %s\n" (Int_ary.to_json_ary (Int_ary.sub ids 0 limit));
	  failwith (sprintf "lxm id: %d.  IDS not ordered." i)
	end;
	let pops = Matrix.pops matrix i in
	  if not (Int_ary.is_ordered pops) then begin
	    printf "%d : %s\n" i (Lexicon.get lexicon i);
	    printf "  %s\n" (Int_ary.to_json_ary (Int_ary.sub pops 0 limit));
	    failwith (sprintf "lxm id: %d.  POPS not ordered." i)
	  end
    done;
    print_endline "It checks out!"
