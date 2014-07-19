open OUnit
open Entities

let test_suite = "entities_test" >:::
  [
    "down_digits" >::
      (fun () ->
	 let text = "Br&#252;tal Legend, funky 'r': &#174; -- and &#167; and &#223; are both 's'." in
	   assert_equal "Brutal Legend, funky 'r': r -- and s and s are both 's'."
	     (Diacritics.fold (down_digits text));
      );

    "down_amps" >::
      (fun () ->
	 assert_equal "Harold & Maude" (down_amps "Harold &amp; Maude");
	 assert_equal "Harold and Maude" (down_amps "Harold and Maude");
      );

    "down_hellips" >::
      (fun () ->
	 let expected = "60-Second ... Workouts" in
	   assert_equal expected (down_hellips "60-Second ... Workouts");
	   assert_equal expected (down_hellips "60-Second . . . Workouts");
	   assert_equal expected (down_hellips "60-Second &hellip; Workouts");
      );

    "down_quotes" >::
      (fun () ->
	 assert_equal "\"Ideas Y Trucos\"/Practical Ideas Series"
	   (down_quotes "&quot;Ideas Y Trucos&quot;/Practical Ideas Series");
      );

    "down_mdashes" >::
      (fun () ->
	 assert_equal "foo - bar" (down_mdashes "foo &mdash; bar")
      );

    "down_all" >::
      (fun () ->
	 assert_equal "\"Ideas & Trucos\" ...Practical Ideas- Series"
	   (down "&quot;Ideas &amp; Trucos&quot; &hellip;Practical Ideas&mdash; Series");
      );
  ]

let _ =
  Test_helper.run test_suite
