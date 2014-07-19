type t

(**
   Gain access to mmap-ed file.
*)

val open_tbl : String.t -> t
val close : t -> unit

(**
   Fetch data by lexeme_id.
*)

val pops : t -> int -> Int_ary.t
val ids : t -> int -> Int_ary.t
  (** F t lexeme_id *)
