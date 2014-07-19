open ExtList;; open Pcre;; open Printf;; open Util

type t =
    { sort_val       : int
    ; canon          : string
    ; target_id      : int
    ; text           : string
    ; pop_rank       : int
    ; title_only     : string option
    }

let sort_val   t = t.sort_val
let text       t = t.text
let title_only t = t.title_only
let pop_rank   t = t.pop_rank

let create (a:string) (b:string) (c:string) (d:string)
    (e:string) (f:string option) : t =
  { sort_val   = int_of_string a
  ; canon      = b
  ; target_id  = int_of_string c
  ; text       = d
  ; pop_rank   = int_of_string e
  ; title_only = f
  }

let tab_re = regexp "\\t"
let from_line (line:string) : t =
  match split ~rex:tab_re line with
    | [a; b; c; d; e]    -> create a b c d e None
    | [a; b; c; d; e; f] -> create a b c d e (Some f)
    | _ ->
	failwith (sprintf "Wrong format for line in stringfile:\n\t%s" line)

(*
  A single t's pop_rank:      (mpac - assoc_count)
  So its actual assoc_count:  (mpac - t.pop_rank)
*)
let mpac = 10_000_000  (*max_possible_assoc_count*)

let sum_of_assoc_counts ts : int =
  let assoc_count t = mpac - t.pop_rank in
    List.fold_left (fun m t -> m + (assoc_count t)) 0 ts

let browse_pop_rank (_:t) (ts:t list) : int =
  mpac - (sum_of_assoc_counts ts)

let product_pop_rank (most_pop:t) (_:t list) : int =
  most_pop.pop_rank

type pop_rank_fun = t -> t list -> int
let calc_pop_rank_fun (domain_name:string) : pop_rank_fun =
  match domain_name with
    | "browse" -> browse_pop_rank
    | _        -> product_pop_rank

let most_popular_exn : t list -> t = function
  | []    -> raise Not_found
  | t::ts ->
      let lower_rank a b =
	if a.pop_rank < b.pop_rank then a else b
      in
	List.fold_left lower_rank t ts

let are_same_result (a:t) (b:t) : bool =
  a.canon = b.canon

(* ts are presumed to be reverse sorted by target_id.
   if you really wanted to be sure, you could sort the result list.
   but it doesn't really matter if they're sorted.
*)
let target_ids ts : int list =
  List.rev_map (fun t -> t.target_id) ts
