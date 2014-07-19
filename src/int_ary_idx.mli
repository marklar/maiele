type t

val open_tbl : string -> string -> t
  (** dir_name *)
val close : t -> unit

val get : t  -> int -> Int_ary.t
