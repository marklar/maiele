(* Currently NOT IN USE.
 * Extracted from usages_file_rdr.ml.
 *)

module MaxInt = struct
  type t = int
  let cmp = (>=)
end
module PQ = Prio_queue.Make(MaxInt)

let prio_queue_from_list (num:int) (usages:usage list) : int PQ.t =
  let first_x_queue =
    let insert pq usage = PQ.insert pq usage.pop usage.pop in
      List.fold_left insert PQ.empty (List.take num usages)
  in List.fold_left
       (fun q usage -> PQ.maybe_insert_keeping_size q usage.pop usage.pop)
       first_x_queue (List.drop num usages)

let most_popular (num:int) (usages:usage list) : int array =
  Array.of_list (PQ.to_list (prio_queue_from_list num usages))

(*
 *   write_partial_gloss:
 *      takes a 'num'; saves only that many of the most popular.
 *)

(* for use with UPCs *)
let write_partial_gloss (mtx:Matrix_mkr.t) (num:int) (usages:usage list)
    : unit =
  match usages with
    | [] -> ()
    | u :: _ ->
	let pops = most_popular num usages in
	  Matrix_mkr.push mtx pops pops u.lxm
