open Pcre;; open Printf;; open ExtList;; open Util

(*
A Str_query_tree is the logical representation of a query,
before it's applied to any given domain.

Then, for each domain, one creates an Id_query_tree from
the Str_query_tree, where the IDs are for the strings (lexemes).
*)

type t =
  | And of (t * t)
  | Or of (t * t)
  | Not of t
  | Leaf of string
  | Empty_node

exception No_uniq_variants_for_lexeme of string

(*--- USING ---*)

(* debug, log *)
let rec show : t -> string = function
  | And (l, r) -> sprintf "(& %s %s)" (show l) (show r)
  | Or (l, r) -> sprintf "(| %s %s)" (show l) (show r)
  | Not node -> sprintf "(- %s)" (show node)
  | Leaf lxm -> lxm
  | Empty_node -> "<Empty node>"

(*-- CREATING --*)

let and_from_ts : t list -> t = function
  | [] -> failwith
        "Str_query_tree.and_from_ts: there should always be multiple nodes."
  | hd :: tl -> List.fold_left (fun m t -> And (t, m)) hd tl
(* not tail - recursive:
| hd :: tl -> List.fold_right (fun m t -> And (m, t)) tl hd
*)

(* for a single lexeme: one leaf per unique variant. *)
let for_uniq_variants_exn (lexeme: string) : t =
  match List.map (fun s -> Leaf s) (Variant.uniq_variants lexeme) with
  | [] -> raise $ No_uniq_variants_for_lexeme lexeme
  | hd :: tl -> List.fold_left (fun m t -> Or (t, m)) hd tl

let hyphen_re = regexp "-"
let init_hyphen_re = regexp "^-"
let empty_tm = subst ""
let of_lexeme (lxm: string) : t =
  if lxm = "-" then
    Empty_node
  else
    (* pull any '-' off start *)
    let lxm' = replace lxm ~rex: init_hyphen_re ~itempl: empty_tm in
    (* don't split on '-' *)
      let tree =
        let variants_node = for_uniq_variants_exn lxm' in
          match split lxm' ~rex: hyphen_re with  (* DO split on '-' *)
          | [] ->
              failwith (sprintf "of_lexeme: shouldn't be empty %s" lxm')
          | _ :: [] ->
              variants_node
          | segments ->
              Or (variants_node,
                and_from_ts (List.map for_uniq_variants_exn segments))
      in
      (* if there was an init '-', wrap node in a Not. *)
        if lxm' <> lxm then
          Not tree
        else
          tree

let of_string (s: string) : t =
  match Maiele_lexer.lex (Maiele_lexer.Query s) with
  | [] -> Empty_node
  | lxms -> and_from_ts (List.map of_lexeme lxms)

(*
Find all Leaf nodes whose lexemes satisfy predicate p.
Return tuple:
- all such matching lexemes (strings)
- a new Str_query_tree with those Leaf nodes removed
*)
let sans_matches t (p: string -> bool) : (string list * t) =
  let rec f t : (string list * t) = match t with
    | Empty_node -> ([], t)
    
    (* Remove leaf if str matches dom_name. *)
    | Leaf lxm ->
        if p lxm then
          ([lxm], Empty_node)
        else
          ([], t)
    
    (*
    Replace nodes in child tree.
    - If becomes empty, Not node also becomes empty.
    - If non - empty tree, then keep the Not.
    *)
    | Not t' ->
        ( match f t' with
          | (lxms, Empty_node) ->
              (lxms, Empty_node)
          | (lxms, t'') ->
              (lxms, Not t'')
        )
    
    (*
    Replace nodes in child trees.
    - If both go empty, this node becomes empty.
    - If just one, then it replaces this node.
    - If neither, keep both.
    Gather up their matching lexemes (if any).
    *)
    | Or (l, r) | And (l, r) as t' ->
        let ((l_lxms, l'), (r_lxms, r')) = (f l, f r) in
          let tree =
            match (l', r') with
            | (Empty_node, Empty_node) -> Empty_node
            | (Empty_node, r') -> r'
            | (l', Empty_node) -> l'
            | ts -> match t' with
                | Or (_, _) -> Or ts
                | _ -> And ts
          in (l_lxms @ r_lxms, tree)
  in f t
