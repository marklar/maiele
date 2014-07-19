val id : 'a -> 'a
val ($) : ('a -> 'b) -> 'a -> 'b
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (|>)    : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

val (<=>) : int -> int -> int
(* val (<=>) : 'a -> 'a -> int *)
  (** Spaceship, specifically for ints (so faster?). *)

val (<->) : int -> (int * int) -> bool
(* val (<->) : 'a -> ('a * 'a) -> bool *)
  (** Included in?
      if num <-> (low, high) then "included" else "not included".
      Again, specifically for ints, for speed. *)


val range : int -> int -> int list
  (** range low high
      not bi-directional.  does not support steps.  *)

val space_sub : Pcre.substitution
val empty_sub : Pcre.substitution

val rjust : int -> string -> string
val ljust : int -> string -> string
val strip : string -> string
val compact_whitespace : string -> string

val zero_pad : int -> string -> string
val superstr_p : string -> string -> bool
val all_prefixes : string -> string list
val split_all_and_join : string -> string -> string
val string_from_chars : char list -> string

(**-- List --*)
(* val foldl : ('a -> 'a -> 'a) -> 'a list -> 'a *)
val find_map_exn : ('a -> 'b option) -> 'a list -> 'b
val insert_into_sorted_list : 'a -> 'a list -> ('a -> 'a -> int) -> 'a list
val insert_into_top_n : int -> 'a -> 'a list -> ('a -> 'a -> int) -> 'a list
val insert_multi_into_top_n : int -> 'a list -> 'a list -> ('a -> 'a -> int) -> 'a list
val rev_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val sub : 'a list -> int -> int -> 'a list
val uniq : 'a list -> 'a list
  (** Does not maintain order. *)


(**-- Time --*)
val formatted_gmtime    : unit -> string
val formatted_localtime : unit -> string
val time_and_res : (unit -> 'a) -> (float * 'a)
  (** IO.  time_and_res thunk *)
val time_it : string -> (unit -> 'a) -> 'a
  (** IO.  time_it descriptive_string thunk *)


(**-- File --
   args: dir_name file_name
*)
val file_exists_p : string -> string -> bool
val file_size     : string -> string -> int

val tmp_file_name : unit -> string
val rm_file : string -> unit

(**-- Parallelism --*)
val invoke : ('a -> 'b) -> 'a -> (unit -> 'b)
  (** returns future which collects the result of forked process *)
