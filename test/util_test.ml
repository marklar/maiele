open OUnit
open Util

let test_suite = "util_test" >:::
  [
    "find_map_exn" >:: 
      (fun () ->
	 let f = function
		  | i when i > 3 -> Some (i+1)
		  | _ -> None
	 in
	   assert_equal 5
	     (find_map_exn f [0;2;4;6]);
	   assert_raises Not_found
	     (fun () -> find_map_exn f [0;1;2])
      );

    "sub" >::
      (fun () ->
	 let xs = [0;1;2;3;4;5] in
	   assert_equal [2;3] (sub xs 2 2);
	   assert_equal [0;1] (sub xs 0 2);
	   assert_equal [0]   (sub xs (-1) 2);
	   assert_equal []    (sub xs 0 (-1));
      );

    "all_prefixes" >::
      (fun () ->
	 assert_equal ["f"; "fo"; "foo"] (all_prefixes "foo");
	 assert_equal [] (all_prefixes "");
      );

    "insert_into_sorted_list" >::
      (fun () ->
	 assert_equal [1; 3; 5; 6; 7; 9]  (insert_into_sorted_list 6  [1;3;5;7;9] compare);
	 assert_equal [1; 3; 5; 7; 9; 10] (insert_into_sorted_list 10 [1;3;5;7;9] compare);
	 assert_equal [0; 1; 3; 5; 7; 9]  (insert_into_sorted_list 0  [1;3;5;7;9] compare);

	 assert_equal [("b",1); ("a",2); ("c",3)]
	   (insert_into_sorted_list ("a",2) [("b",1); ("c",3)] (fun a b -> compare (snd a) (snd b))); 
      );

    "insert_into_top_n" >::
      (fun () ->
	 let f n x = insert_into_top_n n x [1; 3; 5; 7; 9] compare in
	   assert_equal [1; 6; 3; 5; 7; 9]  (f 2  6);
	   assert_equal [1; 3; 5; 6; 7; 9]  (f 10 6);
	   assert_equal [1; 10; 3; 5; 7; 9] (f 2  10);
	   assert_equal [0; 1; 3; 5; 7; 9]  (f 3  0);
      );

    "insert_multi_into_top_n" >::
      (fun () ->
	 let subjs = [4; 12]
	 and objs  = [1; 3; 5; 7; 9]
	 in
	 let f n = insert_multi_into_top_n n subjs objs compare in
	   assert_equal [4; 12; 1; 3; 5; 7; 9] (f 2);
	   assert_equal [1; 4; 12; 3; 5; 7; 9] (f 3);
	   assert_equal [1; 3; 4; 12; 5; 7; 9] (f 4);
	   assert_equal [1; 3; 4; 5; 12; 7; 9] (f 5);
	   assert_equal [1; 3; 4; 5; 7; 12; 9] (f 6);
	   assert_equal [1; 3; 4; 5; 7; 9; 12] (f 10);
      );

    "str_from_chars" >::
      (fun () ->
	 assert_equal "foobar" (string_from_chars ['f'; 'o'; 'o'; 'b'; 'a'; 'r']);
      );
    
    "split_all_and_join" >::
      (fun () ->
	 assert_equal "" (split_all_and_join "" "-");
	 assert_equal "a-b-c" (split_all_and_join "abc" "-");
	 assert_equal "a[\\-',]?b[\\-',]?u[\\-',]?j[\\-',]?a[\\-',]?b[\\-',]?e[\\-',]?r" (split_all_and_join "abujaber" "[\\-',]?");
      );

    "superstr_p" >::
      (fun () ->
	 assert_equal true  (superstr_p "foo"  "foo");
	 assert_equal true  (superstr_p "fool" "foo");
	 assert_equal false (superstr_p "foo"  "fool");
      );

    "compose" >::
      (fun () ->
	 let operand = 7
	 and f1 i = 3 * i
	 and f2 i = i - 2 in
	 let res = f2 (f1 operand) in
	   assert_equal res (compose f2 f1 operand);
	   assert_equal res ((f2 |> f1) operand)
      );

    "spaceship" >::
      (fun () ->
	 assert_equal (-1) (1 <=> 2);
	 assert_equal 0    (1 <=> 1);
	 assert_equal 1    (2 <=> 1)
      );

    "include" >::
      (fun () ->
	 "2 not in 3-4" @? (not (2 <-> (3,4)));
	 "2 in 1-3"     @? (2 <-> (1,3));
	 "2 in 1-2"     @? (2 <-> (1,2));
	 "2 in 2-3"     @? (2 <-> (2,3))
      );

    "range" >::
      (fun () ->
	 assert_equal [1;2;3] (range 1 3);
	 assert_equal [5;6;7] (range 5 7);
	 assert_equal [-2;-1;0;1;2;3] (range (-2) 3)
      );

    "zero_pad" >::
      (fun () ->
	 assert_equal "0000000123" (zero_pad 10 "123");
	 assert_equal "000123"     (zero_pad 6 "123");
	 assert_raises
	   (Invalid_argument "width (1) narrower than string ('123')")
	   (fun () -> zero_pad 1 "123")
      );

    "strip" >::
      (fun () ->
	 assert_equal
	   "Foo bar."
	   (strip "  Foo bar.  ")
      );

    "compact_whitespace" >::
      (fun () ->
	 assert_equal
	   "Foo bar baz quux."
	   (compact_whitespace "  Foo   bar baz    quux.   ");
	 assert_equal
	   "foo"
	   (compact_whitespace " foo");
	 assert_equal
	   "foo"
	   (compact_whitespace "foo ");
	 assert_equal
	   "foo bar"
	   (compact_whitespace "foo bar");
	 assert_equal
	   "foo bar"
	   (compact_whitespace " foo   bar ");
      );
      
    "uniq strs" >::
      (fun () ->
	 let strs = ["foo"; "bar"; "foo"; "baz"; "quux"; "bar"]
	 and s_u_strs = ["bar"; "baz"; "foo"; "quux"] in
	 let u_strs = uniq strs in
	   assert_equal 4 (List.length u_strs);
	   assert_equal s_u_strs (List.sort compare u_strs)
      );

    "uniq int arrays" >::
      (fun () ->
	 let arrays = [ [|1;2;3|];  [|1;2;3|];  [|2;1;3|] ] 
	 and s_u_arrays = [ [|1;2;3|];  [|2;1;3|] ] in
	 let u_arrays = uniq arrays in
	   assert_equal 2 (List.length u_arrays);
	   assert_equal s_u_arrays (List.sort compare s_u_arrays)
      );

    "time_and_res" >::
      (fun () ->
	 let f () = 3 in
	 let (s,r) = time_and_res f in
	   assert_equal 3 r
      );

    "file tests" >::
      (fun () ->
	 let dir_name = "tmp" and file_name = tmp_file_name () in
	 let path = Filename.concat dir_name file_name in
	 let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o640 in
	   ignore (Unix.write fd "bojacked" 0 2);
	   Unix.close fd;
	   "file should exist" @? (file_exists_p dir_name file_name);
	   assert_equal 2 (file_size dir_name file_name);
	   rm_file path
      )
  ]

let _ =
  Test_helper.run test_suite
