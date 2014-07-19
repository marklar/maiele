(*
  To be shared by all verticals:

    - GluID -> domain_name
      (or use Domain.ids_from_target_id t glu_id -> Int_ary.t (maybe empty))

    - GluID -> UPC

  There are (as of 02Jun10) 9,632,109 GluReleases,
  each with a unique (yes?) UPC.

  Sometimes UPC and EAN are not identical.  EAN may have an extra '0' prefix.
  The occasional '-' also makes things look different.

    - Reverse Index:
        + Lexicon: UPCs
        + Matrix:  UPC_ID -> GluID (just one?)

  Each GluRelease:
    - has a unique UPC/EAN
    - belongs to one Glu
*)

open Util
open Printf
open Mysql
open Pcre

(* from tag_tbl_mkr.ml *)
let get_vertical_and_dbd () : string * Mysql.dbd =
  let (vertical, dbhost, dbport, dbname, dbuser) =
    try
      (Sys.argv.(2), Sys.argv.(3), int_of_string Sys.argv.(4),
       Sys.argv.(5), Sys.argv.(6))
    with _ ->
      failwith
	(sprintf
	   "usage: %s <dir_name> <vertical> <db_host> <db_port> <db_name> <db_user> [<db_pwd>]"
	   Sys.argv.(0))
  and dbpwd =
    try Some Sys.argv.(7)
    with _ -> None
  in
    
  let db_info:Mysql.db = { dbhost = Some dbhost  (* "dbhost" *)
			 ; dbport = Some dbport  (* 3306 *)
			 ; dbname = Some dbname  (* "peeko_mark" *)
			 ; dbuser = Some dbuser  (* "mark" *)
			 ; dbpwd  = dbpwd        (* None *)
			 }
  in
    (vertical, connect db_info)

(*
  ean, upc, glu_id, vertical
*)
let get_db_results () =
  let (vertical, dbd) = get_vertical_and_dbd () in
  let sql =
    sprintf
      "  SELECT gr.ean, gr.upc, g.id
           FROM glu_releases gr, glus g, glu_ufa_writeable as guw
          WHERE g.vertical = '%s'
            AND gr.glu_id  = g.id
            AND guw.glu_id = g.id
            AND !g.is_deprecated AND g.catalog_is_listable AND guw.is_listable
       ORDER BY gr.ean ASC"
      vertical
  in Mysql.exec dbd sql

let non_digit_re = regexp "\\D"
let empty_sub    = subst  ""
let get_code : string option -> string = function
  | Some s -> replace s ~rex:non_digit_re ~itempl:empty_sub
  | None   -> ""

let maybe_proper_len_str (code:string) : string option =
  match code with
    | c when String.length c >= 10 && String.length c <= 15 -> Some c
    | _ -> None

let print_ln (code:string) (glu_id:string) : unit =
  match maybe_proper_len_str code with
    | None -> ()
    | Some c ->
	print_endline $ String.concat "\t" [c; glu_id]

let write_all () : unit =
  iter (get_db_results ())
    ( function      (* string option array *)
	| [|None|] -> ()
	| [|ean_opt; upc_opt; Some glu_id|] ->
	    print_ln (get_code ean_opt) glu_id ;
	    print_ln (get_code upc_opt) glu_id 
	| _ -> failwith "Wrong DB record format"
    )

let write_min_and_max_glu (dir_name:string) : unit =
  let (vertical, dbd) = get_vertical_and_dbd () in
  let sql =
    sprintf
      "SELECT
         MIN(glus.id) AS min_glu_id,
         MAX(glus.id) AS max_glu_id
       FROM
         glu_releases
         INNER JOIN glus
           ON glu_releases.glu_id = glus.id
         INNER JOIN glu_ufa_writeable
           ON glu_ufa_writeable.glu_id = glus.id
       WHERE
             glus.vertical = '%s'
         AND glus.is_deprecated = FALSE
         AND glus.catalog_is_listable = TRUE
         AND glu_ufa_writeable.is_listable = TRUE"
      vertical
  in
    let result = Mysql.exec dbd sql
    in
    match Mysql.fetch result with
      | Some row ->
        match row with
          | [|Some min_glu_id; Some max_glu_id|] ->
            let out_chan = open_out (Filename.concat dir_name "min_max_glu_id")
            in
              output_string out_chan min_glu_id;
              output_string out_chan "\n";
              output_string out_chan max_glu_id;
              close_out out_chan
          | _ -> failwith "Wrong DB record format"
      | _ -> failwith "No min and max glu_id"

let _ =
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith $ sprintf "usage: %s <dir_name> <vertical> <db_host> <db_port> <db_name> <db_user> [<db_pwd>]" Sys.argv.(0)
  in
    write_all ();
    write_min_and_max_glu(dir_name)
