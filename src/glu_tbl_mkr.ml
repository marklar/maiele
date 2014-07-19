open Printf;;  open Util;;  open Mysql

module B    = Idx_file.Bool
module Ints = Tbl_mkr.Make(Idx_file.IntListData)

type arg_info =
    { dir_name : string
    ; host     : string
    ; port     : int
    ; name     : string
    ; tag_name : string
    ; user     : string
    ; pwd      : string option
    }

let get_arg_info args : arg_info =
  let (dir_name, host, port, name, tag_name, user) =
    try (args.(1), args.(2), int_of_string args.(3),
	 args.(4), args.(5), args.(6))
    with _ -> failwith
      (sprintf
	 "usage: %s <dir_name> <db_host> <db_port> <db_name> <tag_db_name> <db_user> [<db_pwd>]"
	 args.(0))
  and pwd = try Some args.(7) with _ -> None
  in { dir_name = dir_name
     ; host     = host
     ; port     = port
     ; name     = name
     ; tag_name = tag_name
     ; user     = user
     ; pwd      = pwd
     }

let get_dbd (arg_info:arg_info) (db_name:string) : Mysql.dbd =
  let db_info:Mysql.db =
    { dbhost = Some arg_info.host  (* "dbhost" *)
    ; dbport = Some arg_info.port  (* 3306 *)
    ; dbname = Some db_name        (* "peeko_glu_tags" *)
    ; dbuser = Some arg_info.user  (* "mark" *)
    ; dbpwd  = arg_info.pwd        (* None *)
    }
  in connect db_info

let make_tag_ids_col (arg_info:arg_info) : unit =
  let dbd = get_dbd arg_info arg_info.tag_name in
  let mkr = Ints.create arg_info.dir_name "glu.tag_ids" in
  let sql = "SELECT glu_id,glu_tag_id FROM glu_tag_assocs ORDER BY glu_id" in
  let result = Mysql.exec dbd sql in
  let rec loop glu_id glu_tag_ids : unit =
    match fetch result with
      | None -> ( match glu_tag_ids with
		    | [] -> ()
		    | _ -> Ints.push mkr $ List.rev glu_tag_ids )
      | Some [|Some glu_id_str; Some glu_tag_id_str|] ->
	  let (glu_id', glu_tag_id) =
	    (int_of_string glu_id_str, int_of_string glu_tag_id_str)
	  in
	    if glu_id' = glu_id then
	      loop glu_id (glu_tag_id::glu_tag_ids)
	    else (
	      Ints.push mkr $ List.rev glu_tag_ids;
	      for i = (glu_id+1) to (glu_id'-1) do
		Ints.push mkr []
	      done;
	      loop glu_id' []
	    )
      | _ -> failwith "Wrong DB record format (tag_ids)."
  in
    loop 1 [];
    Ints.close mkr

let get_int (dbd:Mysql.dbd) (sql:string) : int =
  let results = Mysql.exec dbd sql in
    match fetch results with
      | None -> failwith "Couldn't find int."
      | Some [|Some str|] -> int_of_string str
      | _ -> failwith "DB problem: int."

let get_glyde_storefront_id (dbd:Mysql.dbd) : int =
  let sql = "SELECT id FROM storefronts WHERE name = 'glyde'" in
    get_int dbd sql

let get_max_glu_id (dbd:Mysql.dbd) : int =
  let sql = "SELECT MAX(id) FROM glus" in
    get_int dbd sql

let make_in_stock_col (arg_info:arg_info) : unit =
  let dbd = get_dbd arg_info (arg_info.name) in
  let max_glu_id = get_max_glu_id dbd in
  let mkr = B.create arg_info.dir_name "glu.in_stock.data" in
  let sql =
    sprintf
      "SELECT glu_id FROM glu_sort_caches \
        WHERE storefront_id = %d          \
     ORDER BY glu_id"
      (get_glyde_storefront_id dbd)
  in
  let result = Mysql.exec dbd sql in
  let rec loop glu_id : unit =
    match fetch result with
      | None ->
	  for i = glu_id to max_glu_id do
	    B.push mkr false
	  done
      | Some [|Some str|] ->
	  let glu_id' = int_of_string str in
	    for i = glu_id to (glu_id'-1) do
	      B.push mkr false
	    done;
	    B.push mkr true;
	    loop (glu_id'+1)
      | _ -> failwith "Wrong DB record format (in_stock)."
  in
    loop 1;
    B.close mkr

let _ =
  let arg_info = get_arg_info Sys.argv in
    make_tag_ids_col arg_info;
    make_in_stock_col arg_info

