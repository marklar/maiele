open ExtList;;  open Util

(*
  Not currently in use.

  The idea: give each result a SCORE,
  based on its lexical similarity to the query string
  (using the Levenshtein edit distance).
  
*)

type t = { sim_score : float
	 ; result : Result.t
	 }

let create (sim:float) (result:Result.t) : t =
  { sim_score = sim
  ; result    = result
  }

(* uses Levenshtein distance:  1 / distance  *)
let all_from_query_and_results (q:string) (rs:Result.t list) : t list =
  List.map
    (fun r -> let s = Levenshtein.score q (Result.text r) in create s r)
    rs

module MinFloat = struct
  type t = float
  let cmp = (<=)
end
module PQ = Prio_queue.Make(MinFloat)

let top_x_for_query_and_results (num:int) (q:string) (rs:Result.t list) : t list =
  let queue = 
    let ts = all_from_query_and_results q rs in
    let first_x_queue =
      List.fold_left (fun q t -> PQ.insert q t.sim_score t) PQ.empty (take num ts)
    in List.fold_left
	 (fun q t -> PQ.maybe_insert_keeping_size q t.sim_score t)
	 first_x_queue (drop num ts)
  in PQ.to_list queue

let sim_score t = t.sim_score
let result t    = t.result

let update_in_stock (has_sellable_glu:Result.t -> bool) t : t =
  let r = t.result in
    create t.sim_score (if has_sellable_glu r 
			then Result.with_in_stock r
			else r)
