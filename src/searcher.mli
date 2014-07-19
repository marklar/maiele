(**
   "Main" searcher -- does query for all domains (verticals).
   Json-ize here, not in deamon.
*)

(**
   Provide the domain name.
*)

val domains : Domain.t list
val close : unit -> unit

val domain_of_name : string -> Domain.t

val show : string -> Result.t list -> Pcre.regexp list -> Query_opts.t -> string

val results : Query.t -> string -> Query_opts.t -> Result.t list

val target_ids : string -> int -> Int_ary.t
  (** target_ids domain_name lex_id *)
  
val target_ids_for_query : string -> string -> Query_opts.t -> Int_ary.t list
  (** target_ids_for_query query domain_name opts *)
