open ExtList
open Util
open Printf

type t =
    { name       : string   (* see cfg.ml: Games, Tablets... *)
    ; lexicon    : Lexicon.t
    ; matrix     : Matrix.t
    ; result_tbl : Result_tbl.t
    }

type order = Length | Pop

(*--- PUBLIC ---*)

let create (name:string) (dir:string) : t =
  { name       = name
  ; lexicon    = Lexicon.open_tbl dir "lex"
  ; matrix     = Matrix.open_tbl dir
  ; result_tbl = Result_tbl.open_tbl dir
  }
      

let close t : unit =
  Lexicon.close    t.lexicon;
  Matrix.close     t.matrix;
  Result_tbl.close t.result_tbl

let name t : string = t.name
let lexicon t : Lexicon.t = t.lexicon
let matrix t : Matrix.t = t.matrix
let result_tbl t : Result_tbl.t = t.result_tbl
let product_code_tbl t : Product_code_tbl.t option =
  Result_tbl.product_code_tbl t.result_tbl

let target_ids t (id:int) : Int_ary.t = Result_tbl.target_ids t.result_tbl id
let text t (id:int) : string =
  Result_tbl.text t.result_tbl id

(*
  Given a target (Glu ID, really), return list of results.
*)
let results_for_target t (target_id:int) : Result.t list =
  try
    let ids = Result_tbl.ids_from_target_id t.result_tbl target_id in
      Int_ary.map (Result_tbl.fetch_exn t.result_tbl) ids
  with Invalid_argument s ->
    Logger.warn (sprintf "Invalid argument: %s" s) None;
    []
    
(*
  Given a target (Glu, Charity, or Tag ID),
  return JSON list of matching LSD results.
*)
let results_str_for_target t (target_id:int) : string =
  let res_strs:string list = 
    let ids = Result_tbl.ids_from_target_id t.result_tbl target_id
    and jsonize id = 
      let r = Result_tbl.fetch_exn t.result_tbl id in
	Result.to_unadorned_json r t.name
    in Int_ary.map jsonize ids  (* -> 'a list *) 
  in "[" ^ (String.concat ",\n" res_strs) ^ "]"

(*
  Just show me some results.
  Offset, Limit: obvious.
  Order: Pop or Length (lexical).
*)
let range t (order:order) (off:int) (lim:int) : Result.t list =
  let get_id = match order with
    | Pop    -> Result_tbl.id_from_pop t.result_tbl
    | Length -> (fun id -> id)
  and last = off + lim - 1 in
  let rec loop acc id =
    if id > last then
      acc
    else
      try
	let r = Result_tbl.fetch_exn t.result_tbl (get_id id) in
	  loop (r::acc) (id+1)
      with _ ->
	acc
  in List.rev $ loop [] off
