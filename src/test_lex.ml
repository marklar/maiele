open Printf

let _ = 

  let lexicon = Lexicon.open_tbl "." in

    match (try Sys.argv.(1) with _ -> "") with
      | "" -> print_endline "usage: gimme a lexeme"
      | lxm ->
	  printf "id for '%s': %s\n\n"
	    lxm
	    (String.concat "," (List.map string_of_int (Lexicon.ids lexicon lxm)))
	    (*
	  ;
	  let len = Lexicon.length lexicon in
	    for i = (len-1) downto (len-50) do
	      printf "%d: %s\n" i (Lexicon.get lexicon i)
	    done *)
