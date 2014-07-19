open Util;;  open Printf;;  open ExtList;;  open Mysql

(*

root:peeko_andrew_glu_tags> explain in_stock_glu_ids;
+--------+------------------+------+-----+---------+-------+
| Field  | Type             | Null | Key | Default | Extra |
+--------+------------------+------+-----+---------+-------+
| glu_id | int(10) unsigned | NO   | PRI | NULL    |       |
+--------+------------------+------+-----+---------+-------+
1 row in set (0.01 sec)


root:peeko_andrew_glu_tags> explain glu_tag_assocs;
+------------+------------------+------+-----+---------+----------------+
| Field      | Type             | Null | Key | Default | Extra          |
+------------+------------------+------+-----+---------+----------------+
| id         | int(11) unsigned | NO   | PRI | NULL    | auto_increment |
| glu_id     | int(11) unsigned | NO   | MUL | NULL    |                |
| glu_tag_id | int(11) unsigned | NO   | MUL | NULL    |                |
| created_at | datetime         | YES  |     | NULL    |                |
| updated_at | datetime         | YES  |     | NULL    |                |
+------------+------------------+------+-----+---------+----------------+
5 rows in set (0.00 sec)



For a LS result, fetch tag_ids (target_ids, really).
For tag_ids, get all glu_ids.
For each glu_id, is it in stock?

*)

let db_info:Mysql.db = { dbhost = Some "dbhost"
		       ; dbport = Some 3306
		       (* ; dbname = Some "peeko_andrew_glu_tags" *)
		       ; dbname = Some "peeko_prod_glu_tags_20091105"
		       ; dbuser = Some "root"
		       ; dbpwd  = Some ""
		       }

let dbd:Mysql.dbd = connect db_info

let db_fetch_int_exn (sql:string) : int =
  match fetch (exec dbd sql) with
    | Some [|Some s|] -> int_of_string s
    | _ -> raise Not_found

let total_glu_ids (in_str:string) : int =
  let sql =
    sprintf "SELECT SUM(assoc_count) FROM glu_tags WHERE id IN (%s)" in_str
  in db_fetch_int_exn sql

let num_in_stock_glu_ids (in_str:string) : int =
  let sql =
    sprintf
      "SELECT count(gta.glu_id)
         FROM glu_tag_assocs AS gta, in_stock_glu_ids AS isgi
        WHERE glu_tag_id IN (%s)
          AND isgi.glu_id = gta.glu_id"
      in_str
  in db_fetch_int_exn sql
      
let perc (a:int) (b:int) : float =
  (float_of_int a) /. (float_of_int b)

let _ = 
  let rt =
    let dir_name = try Sys.argv.(1) with _ -> "idx/browse" in
      Result_tbl.open_tbl dir_name
  in Result_tbl.iter
       (fun r ->
	  let (num_in_stock, total) =
	    let in_str =
	      String.concat "," $ Int_ary.map string_of_int (Result.target_ids r)
	    in (num_in_stock_glu_ids in_str, total_glu_ids in_str)
	  in printf "%d\t%f\t%d\t%d\t%s\n"
	       (Result.id r)
	       (perc num_in_stock total)
	       num_in_stock
	       total
	       (Result.text r)
       )
       rt
