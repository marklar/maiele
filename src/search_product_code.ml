open Util
open Printf


let glu tbl code =
  match Product_code_tbl.glu_for_code tbl code with
    | None     -> "None"
    | Some glu -> string_of_int glu


let code_glu_pairs tbl code =
  let pairs_list = Product_code_tbl.first_n_code_glu_pairs tbl 10 code in
    sprintf "[ %s ]"
      (String.concat "; "
	 (List.map
	    (fun (s,i) -> sprintf "(%s, %d)" s i)
	    pairs_list))

let _ =
  try
    let code = Sys.argv.(1)
    and tbl = Product_code_tbl.open_tbl "idx/product_codes" in
      print_endline $ "Glu: " ^ (glu tbl code);
      print_endline $ code_glu_pairs tbl code
  with _ ->
    print_endline (sprintf "Usage: %s" Sys.argv.(0))

