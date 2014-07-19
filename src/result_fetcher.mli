type t = unit -> Result.t option

val id_order_fun : Domain.t -> Id_query_tree.t -> t
val pop_order_fun : Domain.t -> Id_query_tree.t -> t