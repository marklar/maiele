(*
  Generator function.
  Given a Domain and an Id_query_tree,
  create a function to generate valid Results.

  It can do so in either of two orders:
     1. res-ID (lexical-length)  -OR-
     2. pop rank

  Works only for SINGLE Matrix.

  Does not support product code searches.  Only title searches.
  (Rename to reflect that?)
*)

open Util;;  open Printf

type t = unit -> Result.t option

(*
  Creates iterator, enclosing over:
    - a Domain's Matrix
    - what Matrix fun to call to get identifiers (res ID / pop rank)
      ("postings": the docs in which a lexeme appears.)
    - Id_query_tree for fetching identifier sets from Matrix

  May raise: Not_found.
*)
let iter_exn (postings_fun:Matrix.t -> int -> Int_ary.t)
    (d:Domain.t) (qt:Id_query_tree.t) : (unit -> int) =
  let f : int -> Int_ary.t =
    postings_fun $ Domain.matrix d
  in
    Id_query_tree.next_val_fun qt f

(*
  'next_id' has 3 behaviors:
     - returns valid Result ID
     - raises Not_found (no more matching results)
     - returns too-large Result ID (in 'Not' node case)

  Perhaps Not node could check its result instead?
*)
let next_valid_res_id (max_id:int) (next_id:unit -> int) : int option =
  try
    let id = next_id () in
      if id > max_id then   (* 'Not' node returns too-large Result ID *)
	None
      else                  (* valid Result ID *)
	Some id
  with Not_found ->         (* No more found *)
    None

(*
  Simply fetch next result ID from the Matrix.
  May raise: Not_found.
*)
let id_fun (d:Domain.t) (qt:Id_query_tree.t) : (unit -> int) =
  iter_exn Matrix.ids d qt

(*
  Fetch next pop rank from the Matrix,
  then return the corresponding result ID.

  May raise: Not_found.
*)
let pop_fun (d:Domain.t) (qt:Id_query_tree.t) : (unit -> int) =
  let pop_iter = iter_exn Matrix.pops d qt
  and to_res_id =
    Result_tbl.id_from_pop (Domain.result_tbl d)
  in
    fun () -> (* unit -> int *)
      to_res_id (pop_iter ())

(*
  Enclose over:
    - which iterator to use (order by ID? pop rank?)
    - the maximum valid Result ID
*)
let order_fun (f:Domain.t -> Id_query_tree.t -> (unit -> int))
    (d:Domain.t) (qt:Id_query_tree.t)
    : t =
  let max_id : int = Result_tbl.length $ Domain.result_tbl d
  and next_id : unit -> int = f d qt
  in
  let rec next () =
	  match next_valid_res_id max_id next_id with
	    | None    -> None
	    | Some id -> Some (Result_tbl.fetch_exn (Domain.result_tbl d) id)
  in next


(*-- exported --*)


let id_order_fun : Domain.t -> Id_query_tree.t -> t =
  order_fun id_fun

let pop_order_fun : Domain.t -> Id_query_tree.t -> t =
  order_fun pop_fun
