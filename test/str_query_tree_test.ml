open OUnit
open Str_query_tree
open Pcre

let t1 = Leaf "matrix"
let t2 = And (Leaf "maude", And (Leaf "harold", Leaf "&"))

let t3 = And (Leaf "superhero",
	      Not (Or (Or (Leaf "spider-man", Leaf "spiderman"),
		       And (Leaf "man", Leaf "spider"))))

let t4 = And (Leaf "jaber", Leaf "abu")
let t5 = Or (Or (Leaf "abujaber", Leaf "abu-jaber"),
	     And (Leaf "jaber", Leaf "abu"))

let t6  = And (Leaf "foo", Leaf "cd")  
let t6' = And (Leaf "foo", Leaf "cd's")  
let t7  = And (Not (Leaf "c"), Leaf "bar")
let t8  = And (And (Leaf "bo", Leaf "baz"), Leaf "quux")
let t8' = And (And (Leaf "book's", Leaf "baz"), Leaf "quux")
let t9  = And (And (Leaf "book", Leaf "bo"), Leaf "foo")

let test_suite = "str_query_tree_test" >:::
  [
    "sans_matches" >::
      (fun () ->
	 let f = sans_matches in
	 let match_p = pmatch ~pat:"c" in
	   assert_equal (["cd"],   Leaf "foo") (f t6  match_p);
	   assert_equal (["cd's"], Leaf "foo") (f t6' match_p);
	   assert_equal (["c"], Leaf "bar")    (f t7  match_p);
	   assert_equal ([], t8)               (f t8  match_p);
	   
	   let match_p = pmatch ~pat:"bo" in
	     assert_equal ([], t6) (f t6 match_p);
	     assert_equal ([], t7) (f t7 match_p);
	     
	     assert_equal (["bo"],     And (Leaf "baz", Leaf "quux")) (f t8  match_p);
	     assert_equal (["book's"], And (Leaf "baz", Leaf "quux")) (f t8' match_p);
	     
	     assert_equal (["book"; "bo"], Leaf "foo")                (f t9  match_p);
      );

    "of_string" >::
      (fun () ->
	 assert_equal t1 (of_string "The Matrix");
	 assert_equal t2 (of_string "Harold & Maude");
	 assert_equal t3 (of_string "superhero -spider-man");
	 assert_equal t4 (of_string "abu jaber");
	 assert_equal t5 (of_string "abu-jaber")
      );

    "show" >::
      (fun () ->
	 assert_equal "matrix" (show t1);
	 assert_equal "(& maude (& harold &))" (show t2);
	 assert_equal "(& superhero (- (| (| spider-man spiderman) (& man spider))))" (show t3);
	 assert_equal "foolish" (show (of_string "Foolish Fool"));
      );
  ]

let _ =   
  Test_helper.run test_suite
