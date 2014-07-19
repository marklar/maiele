open OUnit
open Lexer_util

let srt = List.sort compare

let test_suite = "lexer_util_test" >:::
  [
    "hyphenate_camelcases" >::
      (fun () ->
	 assert_equal
	   "foo-Bar-BAZ"
	   (hyphenate_camelcases "fooBarBAZ");
      );

    "hyphenate_number_word_bounds" >::
      (fun () ->
        assert_equal
        "galaxy s-3"
        (hyphenate_number_word_bounds "galaxy s3");
      );

    "drop_init_article" >::
      (fun () ->
	 assert_equal
	   "Matrix"
	   (drop_init_article "The Matrix");
	 assert_equal
	   "Predator"
	   (drop_init_article "Predator");
	 assert_equal
	   "Room with a View"
	   (drop_init_article "A Room with a View");
      );

    "elide" >::
      (fun () ->
	 assert_equal
	   "02468"
	   (elide "0123456789" (Pcre.regexp "[13579]"))
      );

    "replace_w_space" >::
      (fun () ->
	 assert_equal
	   "spider man"
	   (replace_w_space "spider-man" (Pcre.regexp "-"))
      );

    "rm_blanks" >::
      (fun () ->
	 assert_equal
	   [ "foo"; "bar" ]
	   (rm_blanks [ "foo"; ""; "bar"; "" ])
      );

    "rm_prefix_substrs" >::
      (fun () ->
	 assert_equal
	   (srt ["foolish"; "foodstuffs"; "foon"; "fools"])
	   (srt (rm_prefix_substrs [ "foo"
				   ; "fool"
				   ; "fools"
				   ; "food"
				   ; "foon"
				   ; "foolish"
				   ; "foodstuffs"
				   ]))
      )
  ]

let _ =   
  Test_helper.run test_suite
