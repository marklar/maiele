open OUnit

let test_suite = "diacritics_test" >:::
  [
    "fold" >::
      (fun () ->
	 assert_equal "Almodovar" (Diacritics.fold "Almodóvar");
      );
  ]

let _ =
  Test_helper.run test_suite

