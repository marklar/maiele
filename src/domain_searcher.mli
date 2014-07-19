
type t = Domain.t

val results : t -> Query.t -> Query_opts.t -> Result.t list
  (** For "regular" live search.
      May fetch by both id and pop.
      De-dupes by target_id. *)
