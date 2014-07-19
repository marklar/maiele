(**
  Mmaps file containing 4-byte integers.  (Unsigned, as it happens.)

  (* Get the 5th int from the file:  -> int *)
  let int_ary = Int_ary.from_file some_file_name in
    Int_ary.get int_ary (5-1)

  (* Get ints 5 through 10 from the file:  -> Int_ary.t *)
  let int_ary = Int_ary.from_file some_file_name in
    Int_ary.sub int_ary (5-1) (10-1)

  Also provides:
    iter(), for_all(), map()       -- enumerable-like funcs
    concat(), intersect(), merge() -- the latter 2 ASSUME that the sets are ORDERED!
*)

type t
  
val from_file : ?shared:bool -> string -> string -> t
  (** dir_name file_name *)

val close : t -> unit

val of_array  : int array -> t
val to_array  : t -> int array

val of_list   : int list -> t
val to_list   : t -> int list

val of_length : int -> t
val length    : t -> int
val take : int -> t -> t

val empty     : t
val empty_p   : t -> bool

val eql : t -> t -> bool

(** these are unsafe! *)
  
val get          : t -> int -> int
val unsafe_get32 : t -> int -> int32
val unsafe_get   : t -> int -> int

val set   : t -> int -> int   -> unit
val set32 : t -> int -> int32 -> unit
val safe_set : t -> int -> int -> unit
  (** set t idx value *)
val sub_exn    : t -> int -> int -> t
val unsafe_sub : t -> int -> int -> t

(* should be made private *)
val append : t -> int -> t -> int -> t

val hd   : t -> int
val tl   : t -> t
val last : t -> int

val iter        : (int -> 'a)        -> t -> unit
val iteri       : (int -> int -> 'a) -> t -> unit
val fold_left   : ('a -> int -> 'a)  -> 'a -> t -> 'a
val combine : t -> t -> (int * int) list
val map2 : (int -> int -> 'a) -> t -> t -> 'a list

val is_ordered : t -> bool

(** N.B. Return LISTs. *)
val map         : (int -> 'a) -> t -> 'a list
val filter      : (int -> bool) -> t -> int list

val find        : (int -> bool) -> t -> int option
val index       : t -> int -> int option
val index_where : (int -> bool) -> t -> int option
  (** index_where p t
      Applies predicate p to each value in t until one matches.
      Returns the _index_ of the match. *)
val for_all     : (int -> bool) -> t -> bool
val exists      : (int -> bool) -> t -> bool

val to_json_ary : t -> string
val show        : t -> string


(** multiple Int_ary.t *)
val concat : t list -> t

val max_val_unordered : t -> int
