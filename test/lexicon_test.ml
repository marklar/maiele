open OUnit
open Printf

let dir_name = "tmp"
let file_root = "lex"
let strs =
  List.sort compare
    [ "zoe"     ; "hui-lian"; "maiele"; "zozo"; "chuquiza"; "jiejie"; "jie-jie"
    ; "trevor"  ; "trev"; "trevie"; "trevy"; "hui-feng"; "chuquis"; "didi"; "di-di"; "booboo"
    ; "nerissa" ; "ting"; "tingting"; "punks"; "punker-lu"; "punker"; "dange"; "punky"
    ]
    
let test_suite = "lexicon_test" >:::
  [
    "length, ids" >::
      (fun () ->
	 let lxn = Lexicon.open_tbl dir_name "lex" in
	   assert_equal (List.length strs) (Lexicon.length lxn);
	   assert_equal [6; 5]           (Lexicon.ids lxn "di");
	   assert_equal [16; 15; 13]     (Lexicon.ids lxn "pun");
	   assert_equal [22; 21; 19; 17] (Lexicon.ids lxn "t")
      );

    "get" >::
      (fun () ->
	 let lxn = Lexicon.open_tbl dir_name "lex" in
	 let rec loop idx = function [] -> () | hd :: tl ->
	   assert_equal hd (Lexicon.get_exn lxn idx);
	   loop (idx+1) tl
	 in loop 1 strs
      );

    "bound_ids" >::
      (fun () ->
	 let lxn = Lexicon.open_tbl dir_name "lex" in
	   assert_equal (Some (4, 7))   (Lexicon.bound_ids lxn (Char.code 'd'));
	   assert_equal (Some (13, 17)) (Lexicon.bound_ids lxn (Char.code 'p'));
	   assert_equal None (Lexicon.bound_ids lxn (Char.code 'a'));
	   assert_equal None (Lexicon.bound_ids lxn (Char.code 'x'));
	   assert_equal None (Lexicon.bound_ids lxn 200)
      );
  ]

(**
1       booboo
2       chuquis
3       chuquiza
4       dange
5       di-di
6       didi
7       hui-feng
8       hui-lian
9       jie-jie
10      jiejie
11      maiele
12      nerissa
13      punker
14      punker-lu
15      punks
16      punky
17      ting
18      tingting
19      trev
20      trevie
21      trevor
22      trevy
23      zoe
24      zozo
*)

module TM = Tbl_mkr.Make(Idx_file.StringData)

let _ =
  (* create the word list (without idx) *)
  TM.write_all dir_name file_root strs;
  (* create its idx *)
  ignore (Unix.system (sprintf "./bin/mk_lexicon_idx %s" dir_name));  (* pwd: maiele root *)
  
  (* show strings *)
  (* let i = ref 1 in List.iter (fun s -> printf "%d\t%s\n" !i s; incr i) strs; *)
  
  (* run test suite *)
  let r = Test_helper.run test_suite in
    (* clean up *)
    Util.rm_file (Filename.concat dir_name (file_root ^ "*"));
    r
