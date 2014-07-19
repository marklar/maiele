val log : string -> unit
val log_time : string -> (unit -> 'a) -> 'a
  (** log_time descriptive_string thunk *)
