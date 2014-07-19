type format = [ `Json | `Xml | `Html ]

(** Convert strings: UTF-8 <-> ISO-8859-1. *)
val to_utf8   : string -> string
val to_latin1 : string -> string

exception Invalid_param of string * string
exception Http_method_unsupported of string
exception Invalid_path of string

val format : Http_types.request -> format

val str_param : Http_types.request -> string -> string -> string list -> string
  (** str_param (req:Http_types.request) (name:string) (default:string) (range:string list) *)

val int_param : Http_types.request -> string -> int
val float_param : Http_types.request -> string -> float
  (** req -> name *)

val bool_param : Http_types.request -> string -> bool

val query_str : Http_types.request -> string

val domain_name  : Http_types.request -> string      -> string
val domain_names : Http_types.request -> string list -> string list
  (** supports either 'domain' or 'domains' *)

val show_size : Http_types.request -> int -> int
val offset    : Http_types.request -> int -> int
val limit     : Http_types.request -> int -> int
val jsonp     : Http_types.request -> int -> int