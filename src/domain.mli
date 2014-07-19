(**
   Searcher for a single domain (vertical).
*)

type t
type order = Length | Pop

val create : string -> string -> t
  (** create name dir *)
val close : t -> unit

(**
   Attributes.
*)
val name       : t -> string
val lexicon    : t -> Lexicon.t
val matrix     : t -> Matrix.t
val result_tbl : t -> Result_tbl.t

(**
   Fetching data.
*)

val target_ids : t -> int -> Int_ary.t
  (** For a single result.  For creating line-up. *)

val text : t -> int -> string
  (** For a given id, return the text (e.g. title). *)

val product_code_tbl : t -> Product_code_tbl.t option

val results_for_target : t -> int -> Result.t list
val results_str_for_target : t -> int -> string

val range : t -> order -> int -> int -> Result.t list
