(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)


(* val result_ids : Http_types.request -> string *)
(** Logically a GET, but we use HTTP POST,
    because we're passing large amounts of data (filter res_ids).
    Returns JSON string.
*)

val target_id_2_result_ids_json : Http_types.request -> string
(** Returns JSON string, of course. *)
