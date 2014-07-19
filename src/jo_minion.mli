(* val main : Unix.sockaddr -> string -> unit *)
(*   (** main (join_addr:Unix.sockaddr) (master_chan_name:string) *) *)
val main : int -> string -> unit
  (** main (join_port:int) (master_chan_name:string) *)

val channel : unit ->
  (Str_query_tree.t * string * int * int * int * bool * 
     (Str_query_tree.t * float * (string * Result.t list)) Join.chan)
    Join.chan

val create_n : int -> int -> string -> 'a list
(* val chans : int -> int -> string -> ((Str_query_tree.t * float * (string * Result.t list)) Join.chan) list *)
