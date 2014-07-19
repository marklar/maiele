open OUnit

(**
http://www.xs4all.nl/~mmzeeman/ocaml/ounit-doc/OUnit.html

    type test_result =
      |RSuccess of path
      |RFailure of path * string
      |RError of path * string
      |RSkip of path * string
      |RTodo of path * string
   The possible results of a test

  val run_test_tt : ?verbose:bool -> test -> test_result list
  A simple text based test runner. It prints out information during the test.
*)

let run test_suite =
  let results = run_test_tt test_suite ~verbose:true in
    match List.for_all (function RSuccess _ -> true | _ -> false) results with
      | true -> exit 0
      | _ ->    exit 42

(* To use:
   "bobo" >:: Test_helper.fail;
*)
let fail () = assert_equal 0 1
