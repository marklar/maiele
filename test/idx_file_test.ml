open OUnit
open Util

let dir_name = "tmp" and file_name = Util.tmp_file_name()
(* let create () = IF.create dir_name file_name *)

let test_suite = "idx_file_test" >:::
  [
    "push_int" >::
      (fun () ->
       IF = 
	 let f = create() in
	   "file should exist" @? (Util.file_exists_p dir_name file_name);
	   assert_equal 0 (Util.file_size dir_name file_name);
	   IF.push_int f 123;
	   assert_equal 4 (IF.pos f);
	   IF.close f;
	   assert_equal 4 (Util.file_size dir_name file_name)
      );

    "push String" >::
      (fun () ->
	 let s1 = "janky" in
	 let len1 = String.length s1
	 and s2 = "bojacked" in
	 let len2 = String.length s2
	 and f = create() in
	   IF.push f (IF.String s1);
	   assert_equal len1 (IF.pos f);
	   IF.push f (IF.String s2);
	   assert_equal (len1+len2) (IF.pos f);
	   IF.close f;
	   assert_equal (len1+len2) (Util.file_size dir_name file_name)
      );

    "push Bool" >::
      (fun () ->
	 let bools = [true; false; true; true; false] in
	 let len = List.length bools
	 and f = create() in
	   List.iter (fun b -> IF.push f (IF.Bool b)) bools;
	   assert_equal len (IF.pos f);
	   IF.close f;
	   assert_equal len (Util.file_size dir_name file_name)
      );

    "push IntList" >::
      (fun () ->
	 let ints = [1;2;3;4;5] in
	 let len = (List.length ints) * 4
	 and f = create() in
	   IF.push f (IF.IntList ints);
	   assert_equal len (IF.pos f);
	   IF.close f;
	   assert_equal len (Util.file_size dir_name file_name)
      );

    "push IntArray" >::
      (fun () ->
	 let ints = [|1;2;3;4;5|] in
	 let len = (Array.length ints) * 4
	 and f = create() in
	   IF.push f (IF.IntArray ints);
	   assert_equal len (IF.pos f);
	   IF.close f;
	   assert_equal len (Util.file_size dir_name file_name)
      );

    "push IntAry" >::
      (fun () ->
	 let ints = Int_ary.of_array [|1;2;3;4;5|] in
	 let len = (Int_ary.length ints) * 4
	 and f = create() in
	   IF.push f (IF.IntAry ints);
	   assert_equal len (IF.pos f);
	   IF.close f;
	   assert_equal len (Util.file_size dir_name file_name)
      )

  ]


(* Test Runner *)

let _ =
  let r = Test_helper.run test_suite in
    Util.rm_file (Filename.concat dir_name file_name);
    r
