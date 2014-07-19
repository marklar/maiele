open Util;;  open Printf;;  open ExtList

let show (r:Result.t) : string =
  sprintf "%s\t\t%s" (Result.text r) (String.concat "," (Result.tag_type_strs r))

let uniq_tag_strs (r:Result.t) : string list =
  uniq (Result.tag_type_strs r)

let _ =
  let h = Hashtbl.create 16
  and rt =
    let dir_name =
      try Sys.argv.(1)
      (* with _ -> failwith (sprintf "Usage: %s <dir_name>" Sys.argv.(0)) *)
      with _ -> "idx/browse"
    in Result_tbl.open_tbl dir_name
  in
    (*
      printf "%d\n" (Result_tbl.length rt);
      exit 0;
    *)
    Result_tbl.iter
      (fun r ->
	 let tags = uniq_tag_strs r in
	   if List.length tags > 1 then
	     let tags_str = String.concat ", " (List.sort tags) in
	     let texts = try Hashtbl.find h tags_str with Not_found -> [] in
	       Hashtbl.replace h tags_str (Result.text r :: texts)
      )
      rt;
    printf "%s\t%s\t%s\n" (rjust 10 "-RESULTS-") (ljust 30 "-TAG TYPES-") "-EXAMPLES-";
    let len_str_list = ref [] in
      Hashtbl.iter (fun tags_str texts ->
		      len_str_list := (List.length texts, tags_str) :: !len_str_list;
		   ) h;
      List.iter
	(fun (len, tags_str) ->
	   let examples_str = String.concat " || "
	     (List.map (sprintf "\"%s\"") (List.take 3 (Hashtbl.find h tags_str)))
	   in printf "%s\t%s\t%s\n" (rjust 10 (string_of_int len)) (ljust 30 tags_str) examples_str
	)
	(List.sort ~cmp:(fun a b -> compare b a) !len_str_list)

