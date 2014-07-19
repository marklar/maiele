
open Printf;;

let dir = "games" in
let mtx = Matrix.open_partial_tbl dir 0 in
  for id = 1 to 1000 do
    printf "**** %d\n" id;
    Int_ary.iter (printf "  %d\n") (Matrix.lex_ids mtx id);
    print_endline ""
  done;;


let total = ref 0 in
  for mtx_num = 0 to 4 do
    let dir = "games" in
    let mtx = Matrix.open_partial_tbl dir mtx_num in
    let pop_ids = Matrix.pop_ids mtx 50 in
    let size = Int_ary.length pop_ids in
      printf "len: %d\n" size;
      total := !total + size;
      Int_ary.iter (printf " %d\n") pop_ids
  done;
  printf "total: %d\n" !total
