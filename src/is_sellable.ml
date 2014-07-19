open Util;;  open Printf;;  open Mysql;;  open Logger

(* Perform DB query to find which Glu IDs are sellable. *)
let filter_is_sellable (glu_ids:int list) : Mysql.result =
	let sql =
	  sprintf
	    "SELECT DISTINCT glus.id AS glu_id \
         FROM glus \
              INNER JOIN glu_ufa_writeable \
              ON glus.id = glu_ufa_writeable.glu_id \
        WHERE glus.catalog_is_listable = 1 \
              AND glu_ufa_writeable.is_listable = 1 \
              AND glus.catalog_is_sellable = 1 \
              AND glu_id in (%s)"
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

(* May raise. *)
(* Filter glu_ids, by querying DB to see which are sellable. *)
let all_sellable_glu_ids (glu_ids:int list) : int list =
  match glu_ids with
    | [] -> []
    | _  -> gather_glu_ids (filter_is_sellable glu_ids)

(* Not using Set -- Hash is faster and we need neither ordering nor immutability. *)
let mem_fun (l:int list) : (int -> bool) =
  let h = Hashtbl.create (List.length l) in
    List.iter (fun k -> Hashtbl.add h k 1) l;
    Hashtbl.mem h

(* gather up all Glu IDs for the results *)
let all_glu_ids : Result.t list -> int list =
  Int_ary.to_list |> Ord_int_ary.merge |> List.map Result.target_ids

(*-- public --*)

let has_sellable_glu_fun (rs:Result.t list) : (Result.t -> bool) =
  (* first gather up all results and find the glu_ids.  MAY RAISE.
     then enclose over that w/ function.
     the function's job is to update a list of results... *)
  match all_sellable_glu_ids (all_glu_ids rs) with
    | []      -> (fun _ -> false)  (* value doesn't matter.  won't get used. *)
    | glu_ids -> (Int_ary.exists (mem_fun glu_ids) |> Result.target_ids)
