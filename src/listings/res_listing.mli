(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

type t

(** Create one in memory. *)
val make : string -> int -> int -> t
  (** make domain_name res_id listing_id *)

(** Act on entire DB table. *)
val drop_tbl   : Mysql.dbd -> unit
val create_tbl : Mysql.dbd -> unit

(** Modify DB records. *)
val delete : t -> Mysql.dbd -> unit
val insert : t -> Mysql.dbd -> unit
val replace : t -> Mysql.dbd -> unit
val delete_all_for_listing_id : int -> Mysql.dbd -> unit

(** Special selects. *)

val res_ids_for_listings : int list -> Mysql.dbd -> Int_ary.t
  (** Provide list of listing IDs.  Get back corresponding res IDs. *)

val res_2_listings_for_listing_ids : int list -> Mysql.dbd -> (int, Int_ary.t) Hashtbl.t
  (** Create a hash mapping search results to listings in a collection.
      listing_ids -> dbd -> hash *)
