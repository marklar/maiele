open OUnit
open Util

module ST = Str_tbl
let dir_name = "tmp"
let file_root = Util.tmp_file_name()
let strs = ["hui-feng"; "hui-lian"; "li-ting"]

let test_suite = "str_tbl_test" >:::
  [
    "length, get" >::
      (fun () ->
	 let tbl = ST.open_tbl dir_name file_root in
	   assert_equal (List.length strs) (ST.length tbl);
	   (* this is iter.  below. *)
	   let rec loop idx = function [] -> () | hd :: tl ->
	     assert_equal hd (ST.get_exn tbl idx);
	     loop (idx+1) tl
	   in loop 1 strs
      );

    "iter" >::
      (fun () ->
	 let tbl = ST.open_tbl dir_name file_root in
	   assert_equal (List.length strs) (ST.length tbl);
	   let str_ary = Array.of_list strs in
	   let i = ref 0 in
	     ST.iter (fun s -> assert_equal str_ary.(!i) s;  incr i) tbl;
      );

    "iteri" >::
      (fun () ->
	 let tbl = ST.open_tbl dir_name file_root in
	   assert_equal (List.length strs) (ST.length tbl);
	   let str_ary = Array.of_list strs in
	     ST.iteri (fun i s -> assert_equal str_ary.(i-1) s) tbl
      )
  ]

module TM = Tbl_mkr.Make(Idx_file.StringData)

let _ =
  (* create table *)
  TM.write_all dir_name file_root strs;

  let r = Test_helper.run test_suite in
    (* clean up *)
    rm_file (Filename.concat dir_name (file_root ^ "*"));
    r
      
