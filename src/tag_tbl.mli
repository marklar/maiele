type t

(**
   Specifically for 'browse' index.
   For determining whether to show result
   (based on the number of "available" Glus).
*)

val open_tbl : string -> t
  (**
     open_tbl dir_name
  *)

val close : t -> unit
  (**
     Ensures that mmap-ed fds get closed.
  *)

val fetch_exn : t -> int -> Tag.t
  (**
     For creating a Tag.t.
  *)

val fetch_all_exn : t -> int list -> Tag.t list

val set_avail_glus : t -> int -> int -> unit

val glu_ids : t -> int -> Int_ary.t