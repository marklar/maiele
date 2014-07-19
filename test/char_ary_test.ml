open OUnit
module CA = Char_ary
module SIF = Idx_file.String

let dir_name = "tmp" and file_name = Util.tmp_file_name()

let test_suite = "char_ary_test" >:::
  [
    "all" >::
      (fun () ->
	 let strs = ["contigo a la distancia"; "sabor a mi"; "burbujas de amor"]
	 and f = SIF.create dir_name file_name in
	   List.iter (SIF.push f) strs;
	   SIF.close f;
	   let ca = CA.from_file dir_name file_name in
	   let len = List.fold_left (fun l s -> l + (String.length s)) 0 strs in
	     assert_equal len (CA.length ca);
	     assert_equal "contigo"      (CA.unsafe_get_str ca 0 7);
	     assert_equal "la distancia" (CA.unsafe_get_str ca 10 12);
	     assert_equal "burbujas"     (CA.unsafe_get_str ca 32 8)
      )
  ]

let _ =
  let r = Test_helper.run test_suite in
    Util.rm_file (Filename.concat dir_name file_name);
    r

