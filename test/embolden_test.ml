open OUnit
open Embolden

let test_suite = "embolden_test" >:::
  [
    "embolden" >::
      (fun () ->

	 let html = "Hello (Ben & Jason)"
	 and rex = make_regexps "hello j" in

	   assert_equal
	     "<em>Mass</em> <em>E</em>ffect"
	     (embolden "Mass Effect" (make_regexps "mass e"));

	   assert_equal
	     "<em>Hello</em> (Ben & <em>J</em>ason)"
	     (embolden html rex);

	   assert_equal
	     "<em>Abu-Jaber</em>"
	     (embolden "Abu-Jaber" (make_regexps "abujaber"));
	   
	   assert_equal
	     "Pedro <em>Almodóvar</em> (cds)"
	     (embolden "Pedro Almodóvar (cds)" (make_regexps "almodovar"));

	   assert_equal
	     "<em>Mad</em><em>World</em>"
	     (embolden "MadWorld" (make_regexps "mad world"));
      );
  ]

let _ =
  Test_helper.run test_suite
