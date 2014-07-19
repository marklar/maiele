type query = string
type domain_names = string list
type json = string

(** Must make transparent, so other modules know it's applicable (i.e. a function). *)
type search_fun = query -> domain_names -> int -> int -> int -> bool -> bool -> json

exception Timeout

val serial_search_fun : Logger.t -> search_fun
  (** search_fun logger_fun *)

val parallel_search_fun : int -> string -> Logger.t -> search_fun
  (** parallel_search_fun join_port reg_chan_name logger_fun *)
