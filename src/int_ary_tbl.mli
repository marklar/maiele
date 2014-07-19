(**
   Like a CStore::Partition of data format: 'I*'.
   For now, it's only a reader.  In order to build an index, we'll need funcs for a builder, too.
  
   :: Get Array of ints for record #10:  -> Int_ary.t
   let lex_ids = Int_ary_tbl.open_tbl dir_name "mat.lex" in
   Int_ary_tbl.get_exn lex_ids 10
*)

type t

val open_tbl : string -> string -> t
  (** open_tbl directory_name file_root *)
val close : t -> unit

val length : t -> int
  
val get_exn : t -> int -> Int_ary.t
  (** Indexes start at 1.  If absent, returns empty set. *)

val iter  : (Int_ary.t -> 'a) -> t -> unit
val iteri : (int -> Int_ary.t -> 'a) -> t -> unit

val min_val : t -> int
val max_val : t -> int
