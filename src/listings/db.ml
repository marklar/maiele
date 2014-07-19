(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

open Printf
open Mysql

(*--- SET-UP ---*)

let db_info : Mysql.db = { dbhost = Some "dbhost"
			 ; dbname = Some "test"
			 ; dbport = Some 3306
			 ; dbpwd  = None
			 ; dbuser = Some "mark"
			 }

let dbd = connect db_info


(* I/O *)
let show_status () : unit =
  match status dbd with
    | StatusOK | StatusEmpty -> ()
    | StatusError _ -> match errmsg dbd with
	| None     -> ()
	| Some str -> print_endline str

(* I/O *)
let show_size (res:result) : unit =
  printf "size of res: %Ld\n" (size res)


(*--- PRINCIPAL API ---*)

(* Build specific cases of functor Coll_record,
   passing anonymous module structures adhering to
   the signature Coll_record.Persistible_type.
*)
module Res_ids_record =
  Coll_record.Make (struct
		      type t = Int_ary.t list
		      let tbl_name = "coll_res_ids"
		      let empty_data = []
		    end)

module Res_2_listings_record =
  Coll_record.Make (struct
		      type t = (int, Int_ary.t) Hashtbl.t
		      let tbl_name = "coll_res_2_listings"
		      let empty_data = Hashtbl.create 0
		    end)

(* create tables if not exist *)
let _ = Listing.create_tbl dbd
let _ = Res_listing.create_tbl dbd
let _ = Res_ids_record.create_tbl dbd
let _ = Res_2_listings_record.create_tbl dbd

let coll_res_ids (domain:string) (coll_id:int) : Int_ary.t list =
  Res_ids_record.data_for domain coll_id dbd

let listings_for_res_ids (domain:string) (coll_id:int) (res_ids:Int_ary.t) : Int_ary.t =
  (* Debug.log (sprintf "domain: %s" domain); *)
  let h = Res_2_listings_record.data_for domain coll_id dbd in
  let get_listing_ids lx = try Hashtbl.find h lx with Not_found -> Int_ary.empty in
    (*
      let all = Debug.log_time "getting listing ids" (fun () -> Int_ary.map get_listing_ids res_ids) in
      let xxx = Debug.log_time "just merging listings Int_arys" (fun () -> Int_ary.merge all ) in
    *)
  let all = Int_ary.map get_listing_ids res_ids in
    Int_ary.merge all
    

(* For this domain & collection... *)
let update_ml_tables (domain:string) (coll_id:int) : unit =
  let listing_ids:int list = Listing.ids_for_domain_and_coll domain coll_id dbd in
  let res_ids:Int_ary.t list =
    let res_ids':Int_ary.t =
      Res_listing.res_ids_for_listings listing_ids dbd
    in (* split res_ids' into val ranges, to correspond to matrices. *)
    let sub_in_mtx m = Int_ary.values_in res_ids' (Matrix.min_lex m) (Matrix.max_lex m) in
      List.map sub_in_mtx (Domain.matrices (Searcher.domain_of_name domain))
  in
    Res_ids_record.update domain coll_id res_ids dbd;
    let h = Res_listing.res_2_listings_for_listing_ids listing_ids dbd in
      Res_2_listings_record.update domain coll_id h dbd

(*
 * Delete listing from DB.
 * Remove all res_listing assocs for the listing.
 * Update the ML tables.
 *)
let remove_listing (id:int) : unit =
  match Listing.find id dbd with
    | None   -> ()   (* nothing to do *)
    | Some l -> 
	Listing.delete id dbd;
	Res_listing.delete_all_for_listing_id id dbd;
	update_ml_tables (Listing.domain l) (Listing.coll_id l)


(*
 * Insert listing from DB.
 * Find all results for listing; create result_listing assocs for them.
 * Update the ML tables.
 *)
let add_listing (id:int) (coll_id:int) (domain:string) (target_id:int) : unit =
  Listing.replace (Listing.make id coll_id domain target_id) dbd;
  (* create Res_listings *)
  let res_ids = Searcher.result_ids_for_target_id domain target_id in
    if Int_ary.empty_p res_ids then ()
    else
      let create_assoc res_id =
	let listing = Res_listing.make domain res_id id in
	  Res_listing.replace listing dbd;
      in Int_ary.iter create_assoc res_ids;
	update_ml_tables domain coll_id

(*
 * >> TODO <<
 * 
 * must update all 3 "res"-containing (index-specific) tables:
 *   - results_listings
 *   - coll_res_ids
 *   - coll_res_2_listings
 * so, we 'drop' results_listings, then re-create it.
 *)
let update_domain (domain:string) : unit =
  ()
