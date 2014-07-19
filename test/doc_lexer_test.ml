open OUnit
open Doc_lexer

let test_suite = "doc_lexer_test" >:::
  [
    "canonicalize_str" >::
      (fun () ->
	 assert_equal
	   "matrix revisited"
	   (canonicalize_str "The Matrix Revisited");
	 assert_equal
	   "one of a kind talent"
	   (canonicalize_str "One-of-a-kind Talent");
	 assert_equal
	   (canonicalize_str
	      "The Beading Books Series: Techniques Inspiration & More (book series)")
	   (canonicalize_str
	      "The Beading Books Series: Techniques, Inspiration & More (book series)");
	 assert_equal
	   "almodovar"
	   (canonicalize_str "Almodóvar")
      );


    "all_uniq_variants" >::
      (fun () ->
	 assert_equal
	   ["abu-jaber"; "abujaber"; "jaber"]
	   (List.sort compare (all_uniq_variants "abu-jaber"));
	 assert_equal
	   ["man"; "spider-man"; "spiderman"]
	   (List.sort compare (all_uniq_variants "spider-man"));
	 assert_equal
	   ["are"; "knows"; "man"; "nobody"; "spider-man"; "spiderman"; "who"; "you"]
	   (List.sort compare (all_uniq_variants "spider-man, nobody knows who you are"));

	 (* FIXME -- don't want question marks at end, do we? *)
	 assert_equal
	   ["happenin"; "man?"; "spider-man?"; "spiderman?"; "what's"; "whats"]
	   (List.sort compare (all_uniq_variants "what's happenin', spider-man?"))
      )
  ]

let _ = 
  Test_helper.run test_suite
