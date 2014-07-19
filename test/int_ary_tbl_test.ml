open OUnit
module IAT = Int_ary_tbl

let dir_name = "tmp"
let file_root = "int_ary_tbl_test"
let ias = List.map Int_ary.of_list [ [1;2;3];  [3;4;5];  [2;3;4] ]

let test_suite = "int_ary_tbl_test" >:::
  [
    "length, get" >::
      (fun () ->
	   let tbl = IAT.open_tbl dir_name file_root in
	     assert_equal (List.length ias) (IAT.length tbl);
	     (* this is essentially iteri, as seen below. *)
	     let rec loop idx = function [] -> () | hd :: tl ->
	       assert_equal hd (IAT.get_exn tbl idx) ~cmp:Int_ary.eql;
	       loop (idx+1) tl
	     in loop 1 ias
      );

    "iter" >::
      (fun () ->
	 let tbl = IAT.open_tbl dir_name file_root in
	   IAT.iter (fun ia -> assert_equal 3 (Int_ary.length ia)) tbl
      );

    "iteri" >::
      (fun () ->
	 let tbl = IAT.open_tbl dir_name file_root in
	   (* no need to use Int_ary.eql comparator, as the Int_ary.t's
	      being compared are actually physically equal *)
	   IAT.iteri (fun i ia -> assert_equal ia (IAT.get_exn tbl i)) tbl
      );

    "min_val" >::
      (fun () ->
	 let tbl = IAT.open_tbl dir_name file_root in
	   assert_equal 1 (IAT.min_val tbl);
	   assert_equal 5 (IAT.max_val tbl)
      );
  ]


module TM = Tbl_mkr.Make(Idx_file.IntAryData)

let _ =
  (* create the files *)
  TM.write_all dir_name file_root ias;
  (* run tests *)
  let r = Test_helper.run test_suite in
    (* rm files *)
    Util.rm_file (Filename.concat dir_name (file_root ^ "*"));
    r
