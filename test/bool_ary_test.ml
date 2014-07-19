open OUnit
module BA = Bool_ary
module BIF = Idx_file.Bool

let dir_name = "tmp" and file_name = Util.tmp_file_name()

let test_suite = "bool_ary_test" >:::
  [
    "all" >::
      (fun () ->
	 let bools = [true; false; true; false; true] in
	 let f = BIF.create dir_name file_name in
	   List.iter (BIF.push f) bools;
	   BIF.close f;
	   let ba = BA.from_file dir_name file_name in
	     assert_equal 5 (BA.length ba);
	     let rec loop idx = function [] -> () | hd :: tl ->
	       assert_equal hd (BA.get ba idx);
	       loop (idx+1) tl
	     in loop 0 bools
      )
  ]

let _ =
  let r = Test_helper.run test_suite in
    Util.rm_file (Filename.concat dir_name file_name);
    r
