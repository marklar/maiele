open Util
open Printf
open ExtList
open Pcre

let first_vrai_result (d:Domain.t) (glu_id:int) : Result.t option =
  match Domain.results_for_target d glu_id with
    | [] -> None
    | rs ->
	try
	  Some (List.find (not |> Result.is_faux) rs)
	with Not_found ->
	  failwith "Bogus.  There must exist a regular result for any faux."

let get_result (d:Domain.t) (code:string) (glu_id:int) : Result.t option =
  match first_vrai_result d glu_id with
    | None -> None
    | Some r -> Some (Result.with_display_product_code r code)

(*-- exported --*)

let results_for_domain (d:Domain.t) (query_str:string) (limit:int)
    : Result.t list =
  match Domain.product_code_tbl d with
    | None ->
	Logger.debug "no product code table" None;
	[]
    | Some pct ->
	let code_w_glu_pairs =
	  Product_code_tbl.first_n_code_glu_pairs pct limit query_str
	in
	  Logger.debug (sprintf "number of code/glu pairs: %d" (List.length code_w_glu_pairs)) None;
	  let results =
	    List.filter_map
	      (fun (c,g) -> get_result d c g)
	      code_w_glu_pairs
	  in
	    Logger.debug (sprintf "number of results: %d" (List.length results)) None;
	    results
