(**
   Record in results table.
*)

type t

val create : int -> int -> string -> Int_ary.t -> bool -> string list -> Tag.t list option -> int list option -> t

val sort_by_pop : t list -> t list

val to_unadorned_json : t -> string -> string
  (** Result.to_unadorned_json result domain_name
      No emboldening.  No info about whether in stock.
  *)

val show : t -> Pcre.regexp list -> string -> Query_opts.t -> string

val range_json : t -> string -> string

(* just for result_test.ml *)
val id : t -> int
val pop : t -> int
val text : t -> string
val is_faux : t -> bool
val target_ids : t -> Int_ary.t
val in_stock_p : t -> bool option
val tags : t -> Tag.t list option
val store_ids : t -> int list option

val cmp_pop : t -> t -> int
val too_few_glus_p : t -> bool
val with_in_stock : t -> bool -> t
val add_in_stock_info : (t -> bool) -> t -> t
val add_sellable_info : (t -> bool) -> t -> t


(** product codes *)
val with_display_product_code : t -> string -> t
