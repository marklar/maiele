(**
   Like a CStore::Projection for results.
   Mostly used by Result to gather its data.

   open_tbl(): string -> t

   id_from_pop():  t -> int -> int
*)

type t

val open_tbl : string -> t
val close    : t -> unit

val tag_tbl          : t -> Tag_tbl.t option
val glu_tbl          : t -> Glu_tbl.t option
val product_code_tbl : t -> Product_code_tbl.t option

(** About the whole table. *)
val length         : t -> int

(** About single records. *)
val pop          : t -> int -> int
val text         : t -> int -> string
val target_ids   : t -> int -> Int_ary.t
val id_from_pop  : t -> int -> int
val ids_from_target_id : t -> int -> Int_ary.t

val fetch_exn : t -> int -> Result.t
val iter : (Result.t -> 'a) -> t -> unit
