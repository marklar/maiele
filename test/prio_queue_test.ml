open OUnit

module MaxInt = struct
  type t = int
  let cmp = (>=)
end
module PQ = Prio_queue.Make(MaxInt)

let big_q =
  PQ.insert
    (PQ.insert
       (PQ.insert
	  (PQ.insert
	     (PQ.insert
		(PQ.insert PQ.empty 50 50)
		40 40)
	     30 30)
	  10 10)
       20 20)
    60 60


let test_suite = "prio_queue_test" >:::
  [
    "insert" >::
      (fun () ->
	 let q = PQ.insert (PQ.empty) 10 10 in
	 let (p, _, _) = PQ.extract_exn q in
	   assert_equal 10 p
      );

    "maybe_insert" >::
      (fun () ->
	 assert_equal (50,50)
	   (PQ.peek_exn (PQ.maybe_insert_keeping_size_exn big_q 10 10))
      );

    "peek_exn" >::
      (fun () ->
	 let q = PQ.insert (PQ.insert (PQ.empty) 10 10) 20 20 in
	 let (p, e) = PQ.peek_exn q in
	   assert_equal 20 p;
	   assert_equal 20 e
      );

    "to_list" >::
      (fun () ->
	 assert_equal [10; 20; 30; 40; 50; 60] (PQ.to_list big_q)
      );

    "more_stuff" >::
      (fun () ->
	 let (p, _, q') = PQ.extract_exn big_q in
	   assert_equal 60 p;
	   let (p', _, q'') = PQ.extract_exn q' in
	     assert_equal 50 p';
	     let (p'', _, _) = PQ.extract_exn q'' in
	       assert_equal 40 p''
      );

    "remove_from_empty_raises" >::
      (fun () ->
	 assert_raises
	   PQ.Is_empty
	   (fun () -> PQ.remove_top_exn PQ.empty)
      )
  ]

let _ =
  Test_helper.run test_suite
