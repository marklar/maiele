open OUnit
open Line_info

let t1 = { sort_val   = 30011
	 ; canon      = "foo bar baz"
	 ; target_id  = 2
	 ; text       = "Foo Bar (Baz)"
	 ; pop_rank   = 9_999_990
	 ; title_only = Some "Foo Bar"
	 }

let t2 = { sort_val   = 30011
	 ; canon      = "foo bar baz"
	 ; target_id  = 10
	 ; text       = "Foo Bar (Baz)"
	 ; pop_rank   = 9_999_980
	 ; title_only = Some "Foo Bar"
	 }

let t3 = { sort_val   = 30012
	 ; canon      = "foo bar quux"
	 ; target_id  = 200
	 ; text       = "Foo Bar (Quux)"
	 ; pop_rank   = 100
	 ; title_only = Some "Foo Bar"
	 }

let t4 = { sort_val   = 30012
	 ; canon      = "foo bar quux"
	 ; target_id  = 300
	 ; text       = "Foo Bar (Quux)"
	 ; pop_rank   = 3000
	 ; title_only = Some "Foo Bar"
	 }

(* consed together in "rev" order, just as in str_file_rdr.ml *)
let group_1 = [t2; t1]
let group_2 = [t4; t3]

let test_suite = "line_info_test" >:::
  [
    "from_line" >::
      (fun () ->
	 assert_equal t1 (from_line "00030011\tfoo bar baz\t0000002\tFoo Bar (Baz)\t09999990\tFoo Bar");
	 assert_equal t2 (from_line "00030011\tfoo bar baz\t0000010\tFoo Bar (Baz)\t09999980\tFoo Bar");
      );

    "browse_pop_rank" >::
      (fun () ->
	 assert_equal 9_999_970 (browse_pop_rank t1 group_1);
      );

    "most_popular_exn" >::
      (fun () ->
	 assert_equal t3 (most_popular_exn group_2);
      );

    "product_pop_rank" >::
      (fun () ->
	 let most_pop = most_popular_exn group_2 in
	   assert_equal (pop_rank most_pop) (product_pop_rank most_pop group_2);
      );

    "are_same_result" >::
      (fun () ->
	 assert_equal true (are_same_result t2 t1);
	 assert_equal true (are_same_result t4 t3);
	 assert_equal false (are_same_result t3 t1);
      );

    "target_ids" >::
      (fun () ->
	 assert_equal [2; 10]    (target_ids group_1);
	 assert_equal [200; 300] (target_ids group_2);
      );
  ]

let _ =
  Test_helper.run test_suite
