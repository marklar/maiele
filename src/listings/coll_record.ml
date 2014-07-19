(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

open Printf
open Mysql

(* parameter module SIG. *)
module type Persistible_type = sig
  type t
  val tbl_name : string
  val empty_data : t
end


(* record *)
module type R = sig
  type data
  type t
  val make : string -> int -> data -> t
  val domain : t -> string
  val coll_id : t -> int
  val data : t -> data
  val drop_tbl : Mysql.dbd -> unit
  val create_tbl : Mysql.dbd -> unit
  val insert : t -> Mysql.dbd -> unit
  val replace : t -> Mysql.dbd -> unit
  val find : string -> int -> Mysql.dbd -> t option
  val data_for : string -> int -> dbd -> data
  val update : string -> int -> data -> Mysql.dbd -> unit
  val delete : string -> int -> Mysql.dbd -> unit
end


(* the FUNCTOR.  to be applied to modules below. *)
module Make (Persistible:Persistible_type) = struct
  open Persistible
  type data = Persistible.t

  type t = { domain  : string
	   ; coll_id : int
	   ; data    : data
	   }

  let domain t  = t.domain
  let coll_id t = t.coll_id
  let data t    = t.data

  let make (domain:string) (coll_id:int) (data:data) : t =
    { domain  = domain
    ; coll_id = coll_id
    ; data    = data
    }

  let pickle (data:data) : string  = Marshal.to_string data []
  let unpickle (str:string) : data =
    let (data:data) = Marshal.from_string str 0 in data
  (* let (data:data) = Debug.log_time "unmarshal" (fun () -> Marshal.from_string str 0 )
    in data
  *)

  let tbl_ddl =
    sprintf
      "CREATE TABLE IF NOT EXISTS %s (     \
         domain    char(14)  NOT NULL,     \
         coll_id   int       NOT NULL,     \
         data      longblob  NOT NULL,     \
         UNIQUE KEY (domain, coll_id)      \
       ) ENGINE = InnoDB"
      tbl_name

  let drop_tbl (dbd:dbd) : unit =
    ignore (exec dbd (sprintf "DROP TABLE IF EXISTS %s" tbl_name))
								       
  let create_tbl (dbd:dbd) : unit =
    ignore (exec dbd tbl_ddl)
				  
  (* 0 or 1 *)
  let find (domain:string) (coll_id:int) (dbd:dbd) : t option =
    let res =
      let sql = sprintf
	"SELECT * FROM %s WHERE coll_id = %d AND domain = \"%s\""
	tbl_name coll_id domain
      in exec dbd sql
    in match fetch res with  (* at most 1 res *)
      | None -> None
      | Some [|Some dm; Some cl; Some data|] -> 
	  Some (make dm (int2ml cl) (unpickle data))
      | _ -> raise Not_found

  let data_for (domain:string) (coll_id:int) (dbd:dbd) : data =
    (* match Debug.log_time "fetching & unmarshaling data" (fun () -> find domain coll_id dbd) with *)
    match find domain coll_id dbd with
      | None   -> empty_data
      | Some t -> t.data
	    
  let delete (domain:string) (coll_id:int) (dbd:dbd) : unit =
    try
      let sql =
	sprintf "DELETE FROM %s WHERE domain = \"%s\" AND coll_id = %d"
	  tbl_name domain coll_id
      in ignore (exec dbd sql)
    with _ -> ()
	
  let insert_or_replace t (verb:string) (dbd:dbd) : unit =
    try
      let sql =
	sprintf
	  "%s INTO %s (domain, coll_id, data) VALUES (\"%s\", %d, \"%s\")"
	  verb tbl_name
	  t.domain t.coll_id (escape (pickle t.data))
      in ignore (exec dbd sql)
    with Mysql.Error str -> Debug.log str
    
  let insert t (dbd:dbd) : unit =
    insert_or_replace t "INSERT" dbd

  let replace t (dbd:dbd) : unit =
    insert_or_replace t "REPLACE" dbd

  let update (domain:string) (coll_id:int) (data:data) (dbd:dbd) : unit =
    replace (make domain coll_id data) dbd

end
