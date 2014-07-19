open OUnit
open Util

module IATM  = Tbl_mkr.Make(Idx_file.IntAryData)
module StrTM = Tbl_mkr.Make(Idx_file.StringData)
let dir_name = "tmp"
let file_root = tmp_file_name()

let test_suite = "tbl_mkr_test" >:::
  [
    "all: Int_ary" >::
      (fun () ->
	 let ias = List.map Int_ary.of_list [ [1;2;3];  [3;4;5];  [2;3;4] ]
	 and mkr = IATM.create dir_name file_root in
	   List.iter (IATM.push mkr) ias;
	   IATM.close mkr;
	   "offs file should exist" @? file_exists_p dir_name (file_root ^ ".offs");
	   "data file should exist" @? file_exists_p dir_name (file_root ^ ".data");
	   assert_equal (4 * (List.length ias)) (file_size dir_name (file_root ^ ".offs"));
	   let len = List.fold_left (fun s ia -> s + 4 * (Int_ary.length ia)) 0 ias in
	     assert_equal len (file_size dir_name (file_root ^ ".data"))
      );

    "all: string ary" >::
      (fun () ->
	 let strs = ["Li-Ting"; "Hui-Lian"; "Hui-Feng"]
	 and mkr = StrTM.create dir_name file_root in
	   List.iter (StrTM.push mkr) strs;
	   StrTM.close mkr;
	   "offs file should exist" @? file_exists_p dir_name (file_root ^ ".offs");
	   "data file should exist" @? file_exists_p dir_name (file_root ^ ".data");
	   assert_equal (4 * (List.length strs)) (file_size dir_name (file_root ^ ".offs"));
	   let len = List.fold_left (fun sum str -> sum + (String.length str)) 0 strs in
	     assert_equal len (file_size dir_name (file_root ^ ".data"))
      )
  ]

let _ =
  let r = Test_helper.run test_suite in
    (* use bracketing: http://www.xs4all.nl/~mmzeeman/ocaml/ounit-doc/OUnit.html *)
    rm_file (Filename.concat dir_name (file_root ^ "*"));
    r
