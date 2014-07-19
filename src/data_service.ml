open Util;;  open Printf;;  open Http_types

(* too-specifically named? *)
let target_ids_for_bestseller_import (req:request) : string (*json*) =
  try
    let res = 
      let opts =
	let offset    = Request.offset      req 0 
	and limit     = Request.limit       req 20
	and format    = Request.format      req
	in Query_opts.make 5 offset limit false false false false format
      and query       = Request.query_str   req
      and domain_name = Request.domain_name req "games"
      in
	Searcher.target_ids_for_query query domain_name opts
    in sprintf "[%s]" $ String.concat ", " (List.map Int_ary.to_json_ary res)
  with Request.Invalid_param (name, value) ->
    sprintf
      "Invalid value for argument '%s': '%s'."
      name value

(* 
   Supports multiple versions of request param.
*)
let get_id (req:request) : int option =
  let rec loop = function
    | [] -> None
    | n::names ->
	try Some (int_of_string $ req#param n)
	with Param_not_found _ -> loop names
  in loop ["id"; "lex"; "lex_id"; "res_id"]


(*---- PUBLIC ----*)

let target_ids (req:request) : string (*json*) =
  match get_id req with
    | Some id ->
	(* ID: to create LINEUP *)
	let domain_name = Request.domain_name req "games" in
	  Int_ary.to_json_ary (Searcher.target_ids domain_name id)
    | None ->
	(* QUERY: bestseller import *)
	target_ids_for_bestseller_import req
