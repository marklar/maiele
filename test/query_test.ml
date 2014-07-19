open OUnit
open Util

let code_query s = Query.show (Query.from_string true s)

let test_suite = "query_test" >:::
  [
    "from_string" >::
      (fun () ->
	 (* none, or middle *)
	 assert_equal
	   "code: None.  doc: <Empty node>."
	   $ code_query " - ";
	 assert_equal
	   "code: None.  doc: foo."
	   $ code_query " foo -- ";
	 assert_equal
	   "code: None.  doc: (& siej (& eijes 382))."
	   $ code_query " siej 382 eijes ";

	 (* initial *)
	 assert_equal
	   "code: 0987.  doc: 0987."
	   $ code_query "0987-";

	 assert_equal
	   "code: None.  doc: (& foo (| (| 0987 09-87) (& 87 09)))."
	   $ code_query " 09-87- foo ";

	 (* final *)
	 assert_equal
	   "code: None.  doc: (& foo (| (| 0987 09-87) (& 87 09)))."
	   $ code_query " foo 09-87 ";
      );
    
  ]

let _ =   
  Test_helper.run test_suite
