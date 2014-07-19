type t = Int_ary.t

(**
   ORDERED arrays...

   The following functions all assume
   that the integers are ORDERED (ascending).
   To do: create a new type to ensure that we apply these funcs
   only to objects of such type.
*)

val min_val : t list -> int
val max_val : t list -> int
val min_len : t list -> int

val values_in : t -> int -> int -> t
  (** values_in t low high
      Returns a sub of t with values from low to high (INclusive). *)
  
val intersect  : t list -> t
val merge      : t list -> t

val iter_fun : t -> Iter_fun.t
