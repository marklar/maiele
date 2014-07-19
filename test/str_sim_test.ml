open OUnit
open Str_sim

let a, b, c = ("foo bar", "baz quux", "fooooo")
let xs, ys, zs = (bigrams a, bigrams b, bigrams c)

let test_suite = "str_sim_test" >:::
  [
    "bigrams" >::
      (fun () ->
	 assert_equal ["fo"; "oo"; "ba"; "ar"] xs;
	 assert_equal ["ba"; "az"; "qu"; "uu"; "ux"] ys;
	 assert_equal ["fo"; "oo"; "oo"; "oo"; "oo"] zs;
	 assert_equal [] (bigrams "i o u")
      );

    "union_len" >::
      (fun () -> 
	 assert_equal 9 (union_len xs ys);
	 assert_equal 9 (union_len xs zs)
      );

    "intersection_len" >::
      (fun () ->
	 assert_equal 1 (intersection_len xs ys);  (* "ba" *)
	 assert_equal 2 (intersection_len xs zs);
	 assert_equal 2 (intersection_len zs xs)   (* note order *)
      );

    "cmp_bigrams" >::
      (fun () ->
	 assert_equal (2. *. 1. /. 9.) (cmp_bigrams xs ys);
	 assert_equal (2. *. 2. /. 9.) (cmp_bigrams zs xs);
	 assert_equal (2. *. 2. /. 9.) (cmp_bigrams xs zs)
      );

    "cmp_strings" >::
      (fun () ->
	 assert_equal (2. *. 1. /. 9.) (cmp_strs a b)
      );
  ]

let _ =   
  Test_helper.run test_suite
