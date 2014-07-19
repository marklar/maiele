type query = string
type domain_names = string list
type json = string

(** Must make transparent, so other modules know it's applicable (i.e. a function). *)
type t = query -> domain_names -> Query_opts.t -> json

(* val make : unit -> t *)
val search : t

