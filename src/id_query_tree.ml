open Pcre;;  open Printf;;  open ExtList;;  open Util

(*
  The IDs in an Id_query_tree are for the lexemes
  for that query node.

  From the Id_query_tree, one creates a 'next_val_fun',
  which lazily produces the next matching result key
  (either its record ID or its pop rank).
*)

type t =
  | And  of (t * t)
  | Or   of (t * t)
  | Not  of t
  | Leaf of int list   (* int: lexeme ID *)
  | Empty_node

(*
  Recursively creates tree of ORs, with leaves that iterate over Int_arys.
*)
let rec leaf_fun (ids:int list) (res_ids:int -> Int_ary.t) : Iter_fun.t =
  match ids with
    | []       -> Iter_fun.failed_fun_exn
    | hd :: [] -> Ord_int_ary.iter_fun (res_ids hd)
    | hd :: tl ->
	(* Might it ever be faster to pre-combine these?
	   Or perhaps to memcache the results of doing so
	   when they take a long time?
	*)
	Iter_fun.or_fun_exn
	  (Ord_int_ary.iter_fun (res_ids hd))
	  (leaf_fun tl res_ids)

let rec iter_fun t (res_ids:int -> Int_ary.t) : Iter_fun.t =
  match t with
    | And (l,r)  -> Iter_fun.and_fun_exn (iter_fun l res_ids) (iter_fun r res_ids)
    | Or (l,r)   -> Iter_fun.or_fun_exn  (iter_fun l res_ids) (iter_fun r res_ids)
    | Not node   -> Iter_fun.not_fun     (iter_fun node res_ids)
    | Leaf ids   -> leaf_fun ids res_ids
    | Empty_node -> Iter_fun.failed_fun_exn

let next_val_fun t (res_ids:int -> Int_ary.t) : unit -> int =
  Iter_fun.next_val_fun (iter_fun t res_ids)

(* public:  "translates" query tree for a particular vertical. *)
let from_str_tree (sqt:Str_query_tree.t) (lexicon:Lexicon.t) : t =
  let rec fst : Str_query_tree.t -> t = function
    | Str_query_tree.And (l,r)  -> And (fst l, fst r)
    | Str_query_tree.Or  (l,r)  -> Or  (fst l, fst r)
    | Str_query_tree.Not node   -> Not (fst node)
    | Str_query_tree.Leaf lxm   -> Leaf (Lexicon.ids lexicon lxm)
    | Str_query_tree.Empty_node -> Empty_node
  in fst sqt

(* debug, log *)
let rec show : t -> string = function
  | And (l,r)  -> sprintf "(& %s %s)" (show l) (show r)
  | Or  (l,r)  -> sprintf "(| %s %s)" (show l) (show r)
  | Not node   -> sprintf "(- %s)"    (show node)
  | Leaf ids   -> sprintf "'(%s)"  (String.concat " " (List.map string_of_int ids))
  | Empty_node -> "<Empty node>"
