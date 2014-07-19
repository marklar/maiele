open OUnit
open Maiele_lexer

let srt = List.sort compare

let test_suite = "lexer_test" >:::
  [
    "lex_indexible" >::
      (fun () ->
	 assert_equal
	   ["bar"; "baz?"; "foo"; "foo-bar"]
	   (srt (lex (Doc "-Foo-Bar: Baz?--")))
	   ~msg:"lose word-initial hyphen";
	 assert_equal
	   ["abu"; "abu-jaber"; "jaber"]
	   (srt (lex (Doc "abu-jaber")));
	 assert_equal
	   ["f*o*o"]
	   (srt (lex (Doc "F*o*O")));
	 assert_equal
	   ["bar"; "baz"; "foo"; "foo-bar-baz"]
	   (srt (lex (Doc "fooBarBaz")));
      );
    
    "lex_query" >::
      (fun () ->
	 assert_equal
	   ["abu"; "jaber"]
	   (srt (lex (Query "abu jaber")));
	 assert_equal
	   ["-foo-bar"; "baz?"]
	   (srt (lex (Query"-Foo-Bar: Baz?--")));
	 assert_equal
	   ["f*o*o"]
	   (srt (lex (Query "F*o*O")));
	 assert_equal
	   ["blurfl"; "foo-bar-baz"]
	   (srt (lex (Query "fooBarBaz blurfl")));
      )
  ]

let _ =   
  Test_helper.run test_suite
