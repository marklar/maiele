(**
   Lexicon.  Stores lexemes, each with an ID.
   At query time, the only public funcs we need are:

   open_tbl() :: get a reference to the mmap data.
   ids() :: for a given lexeme, get the ID(s) associated with it.
*)

type t

val open_tbl : string -> string -> t
  (** dir_name *)
val close : t -> unit

val length   : t -> int
val get_exn  : t -> int -> string  (* indexes start at 1 *)
val ids      : t -> string -> int list
  (** list of IDs, in REVERSE order,
      of those match lexemes for 'string' where the match
      isn't a superstring of its precedent.
  *)

val n_matching_ids : t -> int -> string -> int list
  (** Lexeme IDs for next N superstrings of lexeme,
      regardless of whether they're the shortest.
      To be used for UPC/EAN searches.
  *)

val complete_match_id : t -> string -> int option

(* iterator *)
val next_matching_id_fun : t -> string -> (unit -> int option)

(** available for benefit of denormalize_mtx. *)

val shortest_super_ids : t -> string -> int -> int -> int list
  (** in _reverse_ order *)
val bound_ids : t -> int -> (int * int) option
