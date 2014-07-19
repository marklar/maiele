(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

(**
   Update DB tables.
*)

val coll_res_ids : string -> int -> Int_ary.t list
  (** coll_res_ids domain_name coll_id *)

val listings_for_res_ids : string -> int -> Int_ary.t -> Int_ary.t
  
val add_listing : int -> int -> string -> int -> unit
  (** id -> coll_id -> domain_name -> target_id -> unit *)

val remove_listing : int -> unit

val update_ml_tables : string -> int -> unit
  (** domain name -> coll_id -> unit *)

val update_domain : string -> unit
  (** When changing indices, or simply prepping for one for the first time,
      we must blow away any index-specific DB tables (the ones with "res" in them)
      and (re-)create them.
  *)

(**
   I/O...
*)

val show_status : unit -> unit
val show_size : Mysql.result -> unit
