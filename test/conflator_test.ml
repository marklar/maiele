open OUnit

let test_suite = "conflator_test" >:::
  [
    "conflations" >::
      (fun () ->
	 assert_equal []  (Conflator.conflations "foobar");
	 (* assert_equal ["there"; "they're"]
	   (List.sort compare (Conflator.conflations "their")); *)
	 assert_equal ["oz"]        (Conflator.conflations "ounces");
	 assert_equal ["ounces"]    (Conflator.conflations "oz");
	 assert_equal ["3rd"]       (Conflator.conflations "third");
	 assert_equal ["3"; "iii"]    (List.sort compare (Conflator.conflations "three"));
	 assert_equal ["3"; "three"]  (List.sort compare (Conflator.conflations "iii"))
      )
  ]

let _ =
  Test_helper.run test_suite
