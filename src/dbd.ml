(* http://martin.jambon.free.fr/json-wheel-doc/index.html *)
open Json_type;;  open Json_type.Browse;;  open Mysql

(* Using data from JSON file (Cfg.json_tbl),
   extract info about DB connection,
   and create a Mysql.db config struct.

   Database is used for:
     - Searcher to find in-stock Glus
     - anything else?
*)

let default_db_info:Mysql.db =
  { dbname = None
      (* all the rest, from CFG *)
  ; dbhost = Some (string (field Cfg.json_tbl "db_host"))
  ; dbport = ( try match option (field Cfg.json_tbl "db_port") with
		 | None   -> Some 3306
		 | Some s -> Some (int s)
	       with _ -> Some 3306 )
  ; dbuser = Some (string (field Cfg.json_tbl "db_user"))
  ; dbpwd  = ( try match option (field Cfg.json_tbl "db_pwd") with
		 | None   -> None
		 | Some v -> Some (string v)
	       with _ -> None )
  }

(* Should raise if it cannot find the CFG field. *)
let get_dbname (cfg_field:string) : string option =
  Some (string (field Cfg.json_tbl cfg_field))
  
let ufa_db_info:Mysql.db =
  { default_db_info with
      dbname = get_dbname "db_name"
  }

let tag_db_info:Mysql.db =
  { default_db_info with
      dbname = get_dbname "tag_db_name"
  }

(* This indirection seems necessary, but I don't know why.
 * By not connecting to the DB until we actually need to --
 * or perhaps merely not using db_info until we need to? --,
 * we avoid some strange problem with reading the JSON and
 * setting the DB connection config.
 * 
 * Used in get_dbd() below.
 *)
let ufa_dbd_opt:Mysql.dbd option ref = ref None
let tag_dbd_opt:Mysql.dbd option ref = ref None
let max_connection_attempts:int = 5

(*
 * Mysql.ping does not fix broken connections (as advertised).
 * Rather, if the connection is broken, Mysql.ping raises.
 *)
let get_dbd (name:string) : Mysql.dbd =
  let (db_info, dbd_opt) = match name with
    | "ufa" -> (ufa_db_info, ufa_dbd_opt)
    | "tag" -> (tag_db_info, tag_dbd_opt)
    | _     -> raise Not_found in
  let rec attempt (num:int) : Mysql.dbd =
    (* get/create connection *)
    try
      let dbd = match !dbd_opt with
	| Some x -> x   (* get existing *)
	| None ->       (* create new, save it *)
	    let x = connect db_info in  (* may raise *)
	      dbd_opt := Some x;
	      x
      in
	ping dbd;  (* if broken, will raise *)
	dbd

    (* if connect/ping failed... *)
    with e ->
      if num > max_connection_attempts then
	raise e
      else begin
	dbd_opt := None;  (* if was set, now broken *)
	attempt (num+1)
      end
  in
    attempt 0

(* may raise *)
let get_ufa_dbd () : Mysql.dbd = get_dbd "ufa"
let get_tag_dbd () : Mysql.dbd = get_dbd "tag"
