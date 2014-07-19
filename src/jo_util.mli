val get_join_addr : int -> Unix.sockaddr
  (** get_join_addr port_num *)

val timeout : float -> (unit -> 'a) -> 'a option
  (** timeout seconds by_name_fun *)
