open Printf;; open Util;; open Mysql;; open Pcre;;

module Ints = Tbl_mkr.Make(Idx_file.IntListData)
module I = Idx_file.Int

type t = { type_ids_col : I.t
  ; avail_glus_col : I.t
  ; total_glus_col : I.t
  ; glu_ids_col    : Ints.t
}

let create (dir: string) : t =
  let c = I.create dir in
    { type_ids_col = c "tag.type_ids.data"
      ; avail_glus_col = c "tag.avail_glus.data"
      ; total_glus_col = c "tag.total_glus.data"
      ; glu_ids_col    = Ints.create dir "tag.glu_ids"
    }

type record_info =
  {
    type_id : int;
    avail_glus : int;
    total_glus : int;
    glu_ids : int list;
  }

let empty_record_info : record_info =
  {
    type_id = 0;
    avail_glus = 0;
    total_glus = 0;
    glu_ids = [];
  }

type db_record_info =
  {
    tag_id : int;
    tag_type_id : int;
    glu_id_opt : int option;
  }

let create_db_record_info (tag_id_str: string) (tag_type_id_str: string) (glu_id_str_opt: string option) : db_record_info =
  let glu_id_opt = match glu_id_str_opt with
	  | None -> None
	  | Some s -> Some (int_of_string s)
	in
    {
      tag_id = int_of_string tag_id_str;
      tag_type_id = int_of_string tag_type_id_str;
      glu_id_opt = glu_id_opt;
    }

let push t (record_info: record_info) : unit =
  I.push t.type_ids_col record_info.type_id;
  I.push t.avail_glus_col record_info.avail_glus;
  I.push t.total_glus_col record_info.total_glus;
  Ints.push t.glu_ids_col record_info.glu_ids

let close t : unit =
  List.iter I.close [ t.type_ids_col
    ; t.avail_glus_col
    ; t.total_glus_col
    ];
  Ints.close t.glu_ids_col

let get_dbd () : Mysql.dbd =
  let (dbhost, dbport, dbname, dbuser) =
    try (Sys.argv.(2), int_of_string Sys.argv.(3), Sys.argv.(4), Sys.argv.(5))
    with _ -> failwith
          (sprintf
              "usage: %s <dir_name> <db_host> <db_port> <db_name> <db_user> [<db_pwd>]"
              Sys.argv.(0))
  and dbpwd = try Some Sys.argv.(6) with _ -> None in
    let db_info: Mysql.db =
      { dbhost = Some dbhost  (* "dbhost" *)
        ; dbport = Some dbport  (* 3306 *)
        ; dbname = Some dbname  (* "peeko_catalog_glu_tags" *)
        ; dbuser = Some dbuser  (* "mark" *)
        ; dbpwd = dbpwd        (* None *)
      }
    in connect db_info

let sql: string =
  " SELECT
      gt.id,
      gt.glu_tag_type_id,
      gta.glu_id
    FROM
      glu_tags gt
      LEFT OUTER JOIN glu_tag_assocs gta
         ON gt.id = gta.glu_tag_id
    ORDER BY
      gt.id"

let default_avail_glus = 0

let maybe_make_record (mkr:t) (db_record_infos:db_record_info list) : unit =
  match db_record_infos with
    | [] -> ()
    | hd::_ ->
      let glu_ids =
	      let gather_glu_ids acc db_record_info : int list =
	        match db_record_info.glu_id_opt with
	          | None -> acc
	          | Some glu_id -> glu_id::acc
	      in
	        List.fold_left gather_glu_ids [] db_record_infos
      in
        push mkr {
							     type_id = hd.tag_type_id;
							     avail_glus = default_avail_glus;
							     total_glus = List.length glu_ids;
							     glu_ids = glu_ids;
				         }

let _ =
  let mkr = create Sys.argv.(1)
  and idx = ref 1
  and db_results = Mysql.exec (get_dbd ()) sql in
    let rec loop (acc:db_record_info list) : unit =
      match Mysql.fetch db_results with
        | None -> (
          match acc with
            | [] -> ()
            | _ -> maybe_make_record mkr acc
          )
        | Some [| Some tag_id_str; Some tag_type_id_str; glu_id_str_opt |] ->
          let db_record_info = create_db_record_info tag_id_str tag_type_id_str glu_id_str_opt
          in
            if db_record_info.tag_id = !idx then
              loop (db_record_info::acc)
            else (
              maybe_make_record mkr acc;
              incr idx;

		          while !idx < db_record_info.tag_id do
		            push mkr empty_record_info;
	              incr idx
	            done;
              loop (db_record_info::[])
            )
        | _ -> failwith "Wrong DB record format."
	  in
      loop [];
	    close mkr
