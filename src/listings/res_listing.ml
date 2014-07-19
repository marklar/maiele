(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

open Printf
open Mysql

type t = { domain     : string
	 ; res_id     : int
	 ; listing_id : int
	 }

let domain t = t.domain
let res_id t = t.res_id
let lisiting_id t = t.listing_id

let tbl_name = "results_listings"

let tbl_ddl = 
  sprintf
    "CREATE TABLE IF NOT EXISTS %s (            \
       domain      char(14) NOT NULL,           \
       res_id      int      NOT NULL,           \
       listing_id  int      NOT NULL,           \
       UNIQUE KEY (domain, res_id, listing_id), \
       INDEX (domain, listing_id)               \
     ) ENGINE = InnoDB"
    tbl_name

let create_tbl (dbd:dbd) : unit =
  ignore (exec dbd tbl_ddl)

let drop_tbl (dbd:dbd) : unit =
  ignore (exec dbd (sprintf "DROP TABLE IF EXISTS %s" tbl_name))

let make (domain:string) (res_id:int) (listing_id:int) : t =
  { domain     = domain
  ; res_id     = res_id
  ; listing_id = listing_id
  }


(*--- SELECTING ---*)

(*
 * the listing IDs passed in are all from the same domain.
 * plus, listings are only ever in one domain.
 * as such, no need to select from any particular one.
 *)
let res_2_listings_for_listing_ids (listing_ids:int list) (dbd:dbd) : (int, Int_ary.t) Hashtbl.t =
  let hash =
    let approx_hash_size = 2 * (List.length listing_ids) in
      Hashtbl.create approx_hash_size
  and db_res =
    let sql = sprintf
      "SELECT res_id, listing_id FROM %s       \
         WHERE listing_id IN (%s)              \
         ORDER BY res_id ASC, listing_id DESC"    (* DESC: to avoid List.rev *)
      tbl_name
      (String.concat "," (List.map string_of_int listing_ids))
    in exec dbd sql in
  let rec loop (prev_lx_opt:string option) (prev_lst_ids:string list) ary =  (*fetch db_res*)
    match ary with
      | None -> ()
      | Some [|lx_opt; Some lst|] when lx_opt = prev_lx_opt ->
	  loop lx_opt (lst::prev_lst_ids) (fetch db_res)  (* add to cur lx's list *)
      | Some [|lx_opt; Some lst|] ->
	  let _ = match prev_lx_opt with
	    | None -> ()
	    | Some lx ->   (* if have collected any listing_ids, add k:v to hash. *)
		let lst_ids = List.map Int32.of_string prev_lst_ids in
		  Hashtbl.add hash (int_of_string lx) (Int_ary.of_list lst_ids)
 	  in               (* start new list. *)
	    loop lx_opt [lst] (fetch db_res)
      | _ -> raise Not_found in
  let _ = loop None [] (fetch db_res) in
    hash

let res_ids_for_listings (listing_ids:int list) (dbd:dbd) : Int_ary.t =
  let db_res =
    let sql = sprintf
      "SELECT res_id FROM %s         \
         WHERE listing_id IN (%s)    \
         GROUP BY res_id             \
         ORDER BY res_id DESC"   (* DESC: need not List.rev at end. *)
      tbl_name
      (String.concat "," (List.map string_of_int listing_ids))
    in exec dbd sql in
  let rec loop acc ary =  (* fetch db_res *)
    match ary with
      | None -> acc
      | Some [|Some res|] -> loop (res :: acc) (fetch db_res)
      | _ -> raise Not_found (*fixme*) in
  let str_list = loop [] (fetch db_res) in
    Int_ary.of_list (List.map Int32.of_string str_list)


(*--- MODIFYING ---*)		    

let insert_or_replace t (verb:string) (dbd:dbd) : unit =
  let sql =
    sprintf "%s INTO %s (domain, res_id, listing_id) VALUES (\"%s\", %d, %d)"
      verb tbl_name t.domain t.res_id t.listing_id
  in ignore (exec dbd sql)

let insert t (dbd:dbd) : unit =
  insert_or_replace t "INSERT" dbd

let replace t (dbd:dbd) : unit =
  insert_or_replace t "REPLACE" dbd
  
(* needed? *)
let delete t (dbd:dbd) : unit =
  let sql =
    sprintf "DELETE FROM %s WHERE res_id = %d AND listing_id = %d"
      tbl_name t.res_id t.listing_id
  in ignore (exec dbd sql)

let delete_all_for_listing_id (listing_id:int) (dbd:dbd) : unit =
  let sql = sprintf "DELETE FROM %s WHERE listing_id = %d" tbl_name listing_id in
    ignore (exec dbd sql)
