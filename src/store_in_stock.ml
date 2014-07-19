open Util;;  open Printf;;  open Mysql

let filter_has_inventory (store_ids:int list) : Mysql.result =
  let sql =
    sprintf
      "  SELECT storefront_id
           FROM glu_sort_caches
          WHERE storefront_id in (%s)
       GROUP BY storefront_id"
      (String.concat "," (List.map string_of_int store_ids))
  in exec (Dbd.get_ufa_dbd()) sql

(* may raise *)
let all_in_stock_store_ids : int list -> int list = function
  | [] -> []
  | store_ids -> 
      let res = filter_has_inventory store_ids in
      let rec loop acc = function
	| None -> acc
	| Some [|Some store_id|] -> int2ml store_id :: loop acc (fetch res)
	| _ -> raise Not_found (*fixme*)
      in loop [] (fetch res)
	   
let all_store_ids (res:Result.t list) : int list =
  let get_ids r = match Result.store_ids r with
    | None -> []
    | Some ids -> ids
  in List.concat (List.map get_ids res)

(*-- exported --*)

let has_inventory_fun (rs:Result.t list) : (Result.t -> bool) =
  (* first gather up all results and find the store_ids.  MAY RAISE. *)
  (* then enclose over that w/ function to update a list of results... *)
  match all_in_stock_store_ids (all_store_ids rs) with
    | [] -> (fun _ -> false)
    | store_ids ->
	( fun r -> match Result.store_ids r with
	    | None -> false
	    | Some ids -> List.exists (fun i -> List.mem i store_ids) ids )
