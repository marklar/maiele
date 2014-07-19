(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

(** DB record for ML data representing collections.
    
    This module implements a DB record storing an ML data structure.
    The record associates:
      - a collection (of listings) in a single domain (e.g. "game"), with
      - an ML data structure (data) which represents it.

 *)

module type Persistible_type = sig
  type t
  val tbl_name : string
  val empty_data : t
end
  (** Persistible ML data.  Input signature of functor {!Coll_record.Make}. *)



module type R = sig
  (** Types. *)
  type data
    (** The type of the persistible ML data. *)
  type t
    (** The type of record. *)

  val make : string -> int -> data -> t
    (** Make from attribute values. *)

  (** Attr access. *)
  val domain : t -> string
  val coll_id : t -> int
  val data : t -> data

  (** DB table. *)
  val drop_tbl : Mysql.dbd -> unit
  val create_tbl : Mysql.dbd -> unit

  (** CRUD *)
  val insert : t -> Mysql.dbd -> unit
  val replace : t -> Mysql.dbd -> unit
  val find : string -> int -> Mysql.dbd -> t option
  val data_for : string -> int -> Mysql.dbd -> data

  val update : string -> int -> data -> Mysql.dbd -> unit
    (** update domain coll_id data dbd *)

  val delete : string -> int -> Mysql.dbd -> unit
    (** delete domain_name coll_id dbd *)
  
end
  (** Output signature of functor {!Coll_record.Make}. *)



module Make (Persistible:Persistible_type) : R with type data = Persistible.t
  (** Functor building an impl of Coll_record given a Persistible_type. *)
