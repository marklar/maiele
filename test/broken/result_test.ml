open Printf;;

let dir = "./games" in
let res_tbl = Result_tbl.open_tbl dir in
  print_endline ("size of results table: " ^ (string_of_int (Result_tbl.length res_tbl)));
  let res = Result.fetch 1 res_tbl in
    print_endline "result 1.  target_ids...";
    Int_ary.iter (printf "%d\n") (Result.target_ids res);
    (* printf (Result.to_json res); *)
    printf "id:  %d\n" (Result.id res);
    printf "pop: %d\n" (Result.pop res);
    printf "text: %s\n" (Result.text res);
    let res10 = Result.fetch 10 res_tbl in
      print_endline "result 10.  target_ids...";
      Int_ary.iter (printf "%d\n") (Result.target_ids res10);
      let res18 = Result.fetch 18 res_tbl in
	printf "%d\n" (Result.id res18);
