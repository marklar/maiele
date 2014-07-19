

let dir = "./games" in
let query_tree = Query_tree.of_string "mario kart"
and lexicon = Lexicon.open_tbl dir in
let trans_qt = Query_tree.lexemes_to_ids lexicon query_tree in
  List.iter print_endline (Query_tree.lexemes trans_qt)


(* let dir = "../../live_index/peeko_mark/game_product_ls_strings" in *)
(* let query_tree = And [Leaf (Lexeme "ocaml"); Or [Leaf (Lexeme "scien"); Leaf (Lexeme "prog")]] *)
