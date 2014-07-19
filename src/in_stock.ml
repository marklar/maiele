open Util;;  open Printf;;  open Mysql;;  open Logger

(* "Set-once" *)
let glyde_storefront_id_ref = ref None

(*
  If we don't already know the glyde_storefront_id,
  fetch it from the DB, and set the set-once value.
*)
let get_glyde_storefront_id () : int option =
  match !glyde_storefront_id_ref with
    | Some id -> Some id
    | None ->
	let sql = sprintf "SELECT id FROM storefronts WHERE name = '%s'"
	  Cfg.glyde_storefront_name
	in match fetch (exec (Dbd.get_ufa_dbd()) sql) with
	  | Some [|Some id_str|] ->
	      let id = int_of_string id_str in
		glyde_storefront_id_ref := Some id;
		Some id
	  | _ -> None

(* Perform DB query to find which Glu IDs are in stock. *)
let filter_is_in_stock (glu_ids:int list) : Mysql.result =
  match get_glyde_storefront_id() with
    | Some id ->
	let sql =
	  sprintf
	    "SELECT glu_id FROM glu_sort_caches \
        WHERE storefront_id = %d AND glu_id in (%s)"
	    id
	    (String.concat "," (List.map string_of_int glu_ids))
	and dbd = Dbd.get_ufa_dbd()
	in exec dbd sql
    | None ->
	log
	  ("WARNING: Could not find Glyde's storefront ID in DB.  " ^
	     "Might config.yml's :glyde_storefront_name be wrong?")
	  0.;
	raise Not_found

let filter_is_available_for_preorder (glu_ids:int list) : Mysql.result =
  let sql =
    sprintf
      "SELECT DISTINCT glus.id AS glu_id \
         FROM glus \
              INNER JOIN skus \
                ON skus.glu_id = glus.id \
              INNER JOIN pre_order_listings \
                ON pre_order_listings.sku_id = skus.id \
        WHERE pre_order_listings.is_active = 1 \
              AND glus.id in (%s)"
        (String.concat "," (List.map string_of_int glu_ids))
      and dbd = Dbd.get_ufa_dbd()
      in exec dbd sql

(* From DB query results, gather Glu IDs. *)
let gather_glu_ids (db_res:Mysql.result) : int list =
  let rec loop acc = function
    | None -> acc
    | Some [|Some glu_id|] -> int2ml glu_id :: loop acc (fetch db_res)
    | _ -> raise Not_found (*fixme*)
  in loop [] (fetch db_res)

let combine (in_stock_glu_ids:int list) (preorder_glu_ids:int list) : int list =
  let concat = in_stock_glu_ids @ preorder_glu_ids
  in
    let tbl = Hashtbl.create 10 in
		  List.iter (fun i -> Hashtbl.replace tbl i ()) concat;
		  Hashtbl.fold (fun key data accu -> key :: accu) tbl []

(* May raise. *)
(* Filter glu_ids, by querying DB to see which are in stock. *)
let all_in_stock_glu_ids (glu_ids:int list) : int list =
  match glu_ids with
    | [] -> []
    | _  -> combine (gather_glu_ids (filter_is_in_stock glu_ids)) (gather_glu_ids (filter_is_available_for_preorder glu_ids))

(* Not using Set -- Hash is faster and we need neither ordering nor immutability. *)
let mem_fun (l:int list) : (int -> bool) =
  let h = Hashtbl.create (List.length l) in
    List.iter (fun k -> Hashtbl.add h k 1) l;
    Hashtbl.mem h

(* gather up all Glu IDs for the results *)
let all_glu_ids : Result.t list -> int list =
  Int_ary.to_list |> Ord_int_ary.merge |> List.map Result.target_ids


(*-- public --*)

let has_in_stock_glu_fun (rs:Result.t list) : (Result.t -> bool) =
  (* first gather up all results and find the glu_ids.  MAY RAISE.
     then enclose over that w/ function.
     the function's job is to update a list of results... *)
  match all_in_stock_glu_ids (all_glu_ids rs) with
    | []      -> (fun _ -> false)  (* value doesn't matter.  won't get used. *)
    | glu_ids -> (Int_ary.exists (mem_fun glu_ids) |> Result.target_ids)
