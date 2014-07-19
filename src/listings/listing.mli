(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

type t

val make : int -> int -> string -> int -> t
  (** id -> coll_id -> domain -> target_id -> t *)

val coll_id : t -> int
val domain : t -> string


(** Modify DB table. *)

val drop_tbl : Mysql.dbd -> unit
val create_tbl : Mysql.dbd -> unit


(** Modify individual DB records.  Make private? *)

val delete : int (*listing_id*) -> Mysql.dbd -> unit
  (** Listing ID -> DB desc. -> DB desc. *)

val insert : t -> Mysql.dbd -> unit
val replace : t -> Mysql.dbd -> unit


(** Special selects. *)

val find : int -> Mysql.dbd -> t option
  (** Listing Id -> Db desc. -> t option *)

val ids_for_domain_and_coll : string -> int -> Mysql.dbd -> int list
  (** Domain name -> Collection ID -> DB desc. -> Listing IDs *)
