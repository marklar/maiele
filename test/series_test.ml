open OUnit
open Series

let test_suite = "series_test" >:::
  [
    "rm_superfluous_quotes" >::
      (fun () ->
	 let name = "\"\"What Is?\"\" Life Concepts Series, 5" in
	   assert_equal "\"What Is?\" Life Concepts Series, 5" (rm_superfluous_quotes name)
      );

    "rm_surrounding_parens" >::
      (fun () ->
	 assert_equal "some stuff here" (rm_surrounding_parens "(some stuff here)");
	 assert_equal "(some stuff here) and some more" (rm_surrounding_parens "(some stuff here) and some more");
      );

    "rm_repetition_of_series" >::
      (fun () ->
	 assert_equal "something Series" (rm_repetition_of_series "something Series series");
	 assert_equal "something Series" (rm_repetition_of_series "something Series Series");
	 assert_equal "something series" (rm_repetition_of_series "something series Series");
	 assert_equal "something series" (rm_repetition_of_series "something series series");
	 assert_equal "something series" (rm_repetition_of_series "something series series ");
	 assert_equal "something series seriesfoo" (rm_repetition_of_series "something series seriesfoo");
	 assert_equal "fooseries series" (rm_repetition_of_series "fooseries series");
      );

    "rm_final_vert_name" >::
      (fun () ->
	 assert_equal "Some funny" (rm_final_vert_name "games" "Some funny Games")
      );

    "unshout" >::
      (fun () ->
	 assert_equal "Too Loud!" (unshout "TOO LOUD!");
	 assert_equal "KUNG FU fighting" (unshout "KUNG FU fighting");
      );

    "fix" >::
      (fun () ->
	 let ae a b = assert_equal a (fix b "games") in
	   ae "Let'S-Read-And-Find-Out series" "Let'S-Read-And-Find-Out Game";
	   ae "Let'S-Read-And-Find-Out series" "Let'S-Read-And-Find-Out Games";
	   
	   ae "foo bar series" "foo bar series";
	   ae "foo bar series" "foo bar";
	   ae "foo bar series" "foo bar series series";
	   ae "foo bar series" "foo bar game series series";
	   ae "foo & bar series" "foo &amp; bar games";
	   ae "foo & bar series" "foo &amp; bar series series";
	   (* superfluous crap *)
	   ae "Series on Algebra" "\"\"Series on Algebra\"\" series";
	   ae "The Beading Games Series: Techniques, Inspiration & More"
	     "(The Beading Games Series: Techniques, Inspiration & More)";
	   ae "foo & bar series" "&quot;foo &amp; bar&quot; series series";
	   ae "foo & bar series" "(&quot;foo &amp; bar&quot; series series)";
	   ae "Series on Algebra" "\"\"Series on Algebra\"\"";
	   (* shouting *)
	   ae "Foo & Bar series" "(&quot;FOO &amp; BAR&quot; series series)";
	   ae "Series On Algebra" "\"\"SERIES ON ALGEBRA\"\"";
	   ae "Foo Bar series" "FOO BAR game series series";
	   ae "Foo-bar series" "FOO-BAR game series series";
      )
  ]

let _ =
  Test_helper.run test_suite
