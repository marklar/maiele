open OUnit
open Variant

let test_suite = "variant_test" >:::
  [
    "uniq_variants" >::
      (fun () ->
	 assert_equal ["foo-bar"; "foobar"] (List.sort compare (uniq_variants "foo-bar"));
	 assert_equal ["c++"] (uniq_variants "c++");
	 assert_equal ["man"; "man's"; "mans"] (List.sort compare (uniq_variants "man's"));
	 assert_equal ["@"] (uniq_variants "@")
      )
  ]

let _ =   
  Test_helper.run test_suite
