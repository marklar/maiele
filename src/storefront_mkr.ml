(*
  Make store.data file for Charities index.
  For each charity_id (target), store its storefront_id.
*)
open Printf;;  open Util;;  open Mysql

(* from tag_tbl_mkr.ml *)
let get_dbd () : Mysql.dbd =
  let (dbhost, dbport, dbname, dbuser) =
    try (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3), Sys.argv.(4))
    with _ -> failwith
      (sprintf
	 "usage: %s <db_host> <db_port> <db_name> <db_user> [<db_pwd>]"
	 Sys.argv.(0))
  and dbpwd = try Some Sys.argv.(5) with _ -> None in
    
  let db_info:Mysql.db = { dbhost = Some dbhost  (* "dbhost" *)
			 ; dbport = Some dbport  (* 3306 *)
			 ; dbname = Some dbname  (* "peeko_mark" *)
			 ; dbuser = Some dbuser  (* "mark" *)
			 ; dbpwd  = dbpwd        (* None *)
			 }
  in connect db_info

(* from rgs_db.ml *)
let db_fetch_int_exn dbd (sql:string) : int =
  match fetch (exec dbd sql) with
    | Some [|Some s|] -> int_of_string s
    | _ -> raise Not_found

let universal_charity_storefront_id dbd : int =
  db_fetch_int_exn dbd "select id from storefronts where name = 'charities'"

(*
  charity_id, storefront_id
*)
let get_db_results () =
  let dbd = get_dbd () in
  let sql =
    sprintf
      "  SELECT charity_id, storefront_id
           FROM charity_to_storefronts
          WHERE storefront_id != %d
       ORDER BY charity_id ASC"
      (universal_charity_storefront_id dbd)
  in exec dbd sql

(* push N zeroes onto Idx_file.Int *)
let rec zeroes (mkr:Idx_file.Int.t) (n:int) : unit = match n with
  | 0 -> ()
  | _ ->
      Idx_file.Int.push mkr 0;
      zeroes mkr (n-1)

(*
  Loop through DB results (charity_id, storefront_id),
  adding storefront_id in index of corresponding charity_id.
  (If a charity has no storefront, put 0.)
*)
let fill_mkr_with_storefront_ids (mkr:Idx_file.Int.t) : unit =
  let db_results = get_db_results () in
  let rec loop next_charity_id : unit =
    match fetch db_results with
      | None -> ()
      | Some [|Some charity_id_str; Some storefront_id_str|] ->
	  let charity_id    = int_of_string charity_id_str
	  and storefront_id = int_of_string storefront_id_str
	  in
	    zeroes mkr (charity_id - next_charity_id);
	    Idx_file.Int.push mkr storefront_id;
	    loop (charity_id + 1)
      | _ -> failwith "Wrong DB record format."
  in loop 1

let _ =
  let mkr =
    Idx_file.Int.create
      (Filename.concat Cfg.idx_root_dir "charities")
      "store.data"
  in
    fill_mkr_with_storefront_ids mkr;
    Idx_file.Int.close mkr
