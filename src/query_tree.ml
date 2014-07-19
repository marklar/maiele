type t =
  | Str of Str_query_tree.t
  | Id  of Id_query_tree.t

let from_str (s:string) : t =
  Str (Str_query_tree.of_string s)

let show : t -> string = function
  | Str t -> Str_query_tree.show t
  | Id  t -> Id_query_tree.show  t
