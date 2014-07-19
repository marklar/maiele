open OUnit
open Util
module IA = Int_ary
module OIA = Ord_int_ary

let even_p i = i mod 2 = 0

let assert_raises_something (f: unit -> 'a) : unit =
  try begin f(); raise (Failure "shoulda raised something") end
  with _ -> ()

let test_suite = "int_ary_test" >:::
  [
    "combine" >::
      (fun () ->
	 (*
	   let p a_list = 
	   print_endline (String.concat ";"
	   (List.map (fun (a,b) -> Printf.sprintf "(%d,%d)" a b) a_list))
	   in
	 *)
	   (* p (IA.combine (IA.of_array [|1;2;3|]) (IA.of_array [|10;20;30;40;50|])); *)
	 assert_equal
	   [(1,10); (2,20); (3,30)]
	   (IA.combine (IA.of_array [|1;2;3|]) (IA.of_array [|10;20;30;40;50|]));
	   (* p (IA.combine (IA.of_array [|1;2;3|]) IA.empty); *)
	 assert_equal [] (IA.combine (IA.of_array [|1;2;3|]) IA.empty);
      );

    "is_ordered" >::
      (fun () ->
	 assert_equal true  (IA.is_ordered (IA.of_array [|0; 1; 2|]));
	 assert_equal true  (IA.is_ordered (IA.of_array [|0; 2; 2|]));  (* dupes OK *)
	 assert_equal false (IA.is_ordered (IA.of_array [|0; 2; 1|]));
      );

    "to_/of_array" >::
      (fun () ->
	 let ary = [|0; 1; 2|] in
	   assert_equal ary (IA.to_array (IA.of_array ary))
      );

    "to_/of_list" >::
      (fun () ->
	 let list = [0; 1; 2] in
	   assert_equal list (IA.to_list ((IA.of_list list)))
      );

    "of_length/length" >::
      (fun () ->
	 let len = 100 in
	   assert_equal len (IA.length (IA.of_length len));
	   let list = [0; 1; 2; 3] in
	   let ia = IA.of_list list in
	     assert_equal (IA.length ia) (List.length list)
      );

    "empty" >::
      (fun () ->
	 let ia1 = IA.of_length 3 in
	   "should not be empty" @? (not (IA.empty_p ia1));
	   let ia2 = IA.of_length 0 in
	     "should be empty" @? (IA.empty_p ia2)
      );

    "get" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal 0 (IA.get ia 0);
	   assert_equal 1 (IA.get ia 1);
	   assert_equal 2 (IA.get ia 2);
	   assert_raises (Invalid_argument "Int_ary.get: -1") (fun () -> IA.get ia (-1));
	   assert_raises (Invalid_argument "Int_ary.get: 3")  (fun () -> IA.get ia 3)
      );

    "set" >::
      (fun () ->
	 let ia = IA.of_list [0; 1] in
	   assert_equal 0 (IA.get ia 0);
	   IA.set ia 0 1;
	   assert_equal 1 (IA.get ia 1);
	   assert_raises_something (fun () -> IA.set ia (-1) 1);
	   assert_raises_something (fun () -> IA.set ia 4 1)
      );

    "sub_exn" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal (IA.of_list [0; 1]) (IA.sub_exn ia 0 2);
	   assert_equal (IA.of_list [1; 2]) (IA.sub_exn ia 1 2);
	   assert_raises (Invalid_argument "Int_ary.sub_exn. idx, len: 2, 2") (fun () -> IA.sub_exn ia 2 2)
      );

    "hd" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal 0 (IA.hd ia);
	   assert_raises_something (fun () -> IA.hd IA.empty)  (* index out of bounds *)
      );

    "tl" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal [1; 2] (IA.to_list (IA.tl ia));
	   assert_raises (Invalid_argument "Bigarray.sub: bad sub-array") (fun () -> IA.tl IA.empty)
      );
    
    "last" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal 2 (IA.last ia);
	   assert_raises (Invalid_argument "Int_ary.get: -1") (fun () -> IA.last IA.empty)
      );

    "index" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2] in
	   assert_equal (Some 0) (IA.index ia 0);
	   assert_equal (Some 1) (IA.index ia 1);
	   assert_equal (Some 2) (IA.index ia 2);
	   assert_equal None (IA.index ia 3)
      );

    "iter" >::
      (fun () ->
	 let list = [3; 10; 2; 8; 16] in
         let ia = IA.of_list list
	 and len = ref 0 and sum = ref 0 in
	   IA.iter (fun v -> len := !len + 1;  sum := !sum + v) ia;
	   assert_equal !len (List.length list);
	   assert_equal !sum (List.fold_left (+) 0 list)
      );

    "iteri" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2; 3] in
	   IA.iteri (fun i v -> assert_equal i v) ia
      );

    "fold_left" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2; 3] in
	 let lowest = IA.fold_left (fun lo i -> min i lo) max_int ia in
	   assert_equal 0 lowest;
      );

    "map2" >::
      (fun () ->
	 let ia1 = IA.of_list [0;   1;  2;  3]
	 and ia2 = IA.of_list [10; 11; 12; 13] in
	   assert_equal
	     [10; 12; 14; 16]
	     (IA.map2 (fun a b -> a + b) ia1 ia2)
      );

    "filter" >::
      (fun () ->
	 assert_equal
	   [0; 2]
	   (IA.filter (fun a -> a mod 2 = 0) (IA.of_list [0; 1; 2; 3]))
      );

    "iter_fun" >::
      (fun () ->
	 let list = [5; 10; 15; 20] in
	 let ia = IA.of_list list in
	 let next = OIA.iter_fun ia in
	   assert_equal 5  $ next 0;
	   assert_equal 5  $ next 5;  (* won't advance *)
	   assert_equal 10 $ next 8;
	   assert_equal 10 $ next 3;  (* will never revert *)
	   assert_equal 15 $ next 13;
	   assert_equal 20 $ next 18;
	   assert_raises Not_found (fun () -> next 21)
      );

    "map" >::
      (fun () ->
	 let f i = i * 2
	 and list = [5; 10; 15; 20] in
	 let list' = List.map f list
	 and ia = IA.of_list list in
	   assert_equal list' (IA.map f ia)
      );

    "find" >::
      (fun () ->
	 let ia = IA.of_list [10; 100; 9; 67; 1; 20] in
	   assert_equal (Some 1) (IA.find (fun i -> i < 5)    ia);
	   assert_equal None     (IA.find (fun i -> i > 1000) ia)
      );

    "index_where" >::
      (fun () ->
	 let gt5 i = i > 5
	 and ia = IA.of_list [1; 2; 3; 4] in
	   assert_equal (Some 1) (IA.index_where even_p ia);
	   assert_equal None (IA.index_where gt5 ia)
      );

    "for_all" >::
      (fun () ->
	 let lt5 i = i < 5
	 and ia = IA.of_list [1; 2; 3] in
	   "shouldn't all be even"     @? (not (IA.for_all even_p ia));
	   "should all be less than 5" @? (IA.for_all lt5 ia)
      );

    "exists" >::
      (fun () ->
	 let mixed = IA.of_list [1; 2; 3]
	 and odds  = IA.of_list [1; 3; 5] in
	   "should find even" @? (IA.exists even_p mixed);
	   "shouldn't find any even" @? (not (IA.exists even_p odds))
      );

    "values_in" >::
      (fun () ->
	 let ia = IA.of_list [0; 1; 2; 3; 4; 5; 6; 7] in
	   assert_equal (IA.of_list [1; 2; 3; 4]) (OIA.values_in ia 1 4);
	   assert_equal (IA.of_list [5; 6; 7])    (OIA.values_in ia 5 10);
	   assert_equal IA.empty (OIA.values_in ia 100 200);
	   assert_equal ia  (OIA.values_in ia (-10) 100)
      );

    "to_json_ary" >::
      (fun () ->
	 let ia = IA.of_list [1; 2; 3] in
	   assert_equal "[1,2,3]" (IA.to_json_ary ia);
	   assert_equal "[]" (IA.to_json_ary IA.empty)
      );

    "show" >::
      (fun () ->
	 let ia = IA.of_list [1; 2; 3] in
	   assert_equal "[|1; 2; 3|]" (IA.show ia);
	   assert_equal "[||]" (IA.show IA.empty)
      );

    "min_val" >::
      (fun () ->
	 let ia1 = IA.of_list [0; 1; 2]
	 and ia2 = IA.of_list [1; 2; 3] in
	   assert_equal 0 (OIA.min_val [ia1; ia2]);
	   assert_raises_something (fun () -> OIA.min_val [ia1; ia2; IA.empty])
      );

    "max_val" >::
      (fun () ->
	 let ia1 = IA.of_list [0; 1; 2]
	 and ia2 = IA.of_list [1; 2; 3] in
	   assert_equal 3 (OIA.max_val [ia1; ia2]);
	   assert_raises_something (fun () -> OIA.max_val [ia1; ia2; IA.empty])
      );

    "min_len" >::
      (fun () ->
	 let ia1 = IA.of_list [0; 1]
	 and ia2 = IA.of_list [1; 2; 3] in
	   assert_equal 2 (OIA.min_len [ia1; ia2]);
	   assert_equal 2 (OIA.min_len [ia2; ia1]);
	   assert_equal 0 (OIA.min_len [ia1; ia2; IA.empty])
      );

    "concat" >::
      (fun () ->
	 let all = IA.of_list [1; 2; 3; 4]
	 and ia1 = IA.of_list [1; 2]
	 and ia2 = IA.of_list [3; 4]
	 and ia3 = IA.of_list [] in
	   assert_equal all (IA.concat [ia1; ia2; ia3])
      );

    "eql" >::
      (fun () ->
	 let l = [1;2;3] in
	 let ia1 = IA.of_list l and ia2 = IA.of_list l in
	   "should be equal" @? (IA.eql ia1 ia2)
      );

    "merge" >::
      (fun () ->
	 let union = IA.of_list [1; 8; 13; 14; 17; 20]
	 and ia1 = IA.of_list [1; 14]
	 and ia2 = IA.of_list [8; 13; 14; 17]
	 and ia3 = IA.of_list [1; 8; 20]
	 and ia4 = IA.empty in
	 let res = OIA.merge [ia1; ia2; ia3; ia4] in
	   (*
	    * Can't be known whether to hold.  Values may be right, but storage is different.
	    * assert_equal union res;
	    *)
	   assert_equal union res ~cmp:IA.eql;
	   let ia5 = IA.of_list (Util.range 100 5000) in
	   let res = OIA.merge [ia1; ia2; ia3; ia4; ia5] in
	     assert_equal (IA.of_list ([1; 8 ; 13; 14; 17; 20] @ (Util.range 100 5000))) res ~cmp:IA.eql;
      );

    "intersect" >::
      (fun () ->
	 let inter = IA.of_list [10; 20; 30]
	 and ia1 = IA.of_list [8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28; 30; 32; 34]
	 and ia2 = IA.of_list [5; 10; 15; 20; 25; 30; 35; 40]
	 and ia3 = IA.of_list [0; 10; 20; 30; 40] in
	 let res = OIA.intersect [ia1; ia2; ia3] in
	   assert_equal inter res ~cmp:IA.eql
      )

  ]

(* Test Runner *)

let _ =
  Test_helper.run test_suite
