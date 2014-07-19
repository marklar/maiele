(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

open Printf
open Mysql

type t = { id        : int
	 ; coll_id   : int
	 ; domain    : string
	 ; target_id : int
	 }

(* just make type transparent? *)
let id t        = t.id
let coll_id t   = t.coll_id
let domain t    = t.domain
let target_id t = t.target_id

let tbl_name = "listings"

let tbl_ddl =
  sprintf
    "CREATE TABLE IF NOT EXISTS %s (   \
       id         int      NOT NULL,   \
       coll_id    int      NOT NULL,   \
       domain     char(14) NOT NULL,   \
       target_id  int      NOT NULL,   \
       PRIMARY KEY (id)                \
     ) ENGINE = InnoDB"
    tbl_name

(*** TABLE ***)

let drop_tbl (dbd:dbd) : unit =
  ignore (exec dbd (sprintf "DROP TABLE IF EXISTS %s" tbl_name))

let create_tbl (dbd:dbd) : unit =
  ignore (exec dbd tbl_ddl)
    

(*** ROW ***)

let make (id:int) (coll_id:int) (domain:string) (target_id:int) : t =
  { id        = id
  ; coll_id   = coll_id
  ; domain    = domain
  ; target_id = target_id
  }

(* -> _sorted_ *)
let ids_for_domain_and_coll (domain:string) (coll_id:int) (dbd:dbd) : int list =
  let sql =
    sprintf
      "SELECT id FROM %s WHERE domain = \"%s\" AND coll_id = %d ORDER BY id"
      tbl_name domain coll_id in
  let res = exec dbd sql in
  let rec loop acc = function
    | None -> acc
    | Some [|Some id|] -> int2ml id :: loop acc (fetch res)
    | _ -> raise Not_found (*fixme*)
  in loop [] (fetch res)
  

let find (id:int) (dbd:dbd) : t option =
  let res = exec dbd (sprintf "SELECT * FROM %s WHERE id = %d" tbl_name id) in
    match fetch res with
      | None -> None
      | Some [|Some id; Some cl; Some dm; Some trg|] -> 
	  Some (make (int2ml id) (int2ml cl) dm (int2ml trg))
      | _ -> raise Not_found

let delete (id:int) (dbd:dbd) : unit =
  try ignore (exec dbd (sprintf "DELETE FROM %s WHERE id = %d" tbl_name id))
  with _ -> ()

let insert_or_replace t (verb:string) (dbd:dbd) : unit =
  let sql =
    sprintf
      "%s INTO %s                         \
        (id, coll_id, domain, target_id)  \
        VALUES (%d, %d, \"%s\", %d)"
      verb tbl_name
      t.id t.coll_id t.domain t.target_id
  in ignore (exec dbd sql)
    
let replace t (dbd:dbd) : unit =
  insert_or_replace t "REPLACE" dbd

let insert t (dbd:dbd) : unit =
  insert_or_replace t "INSERT" dbd
