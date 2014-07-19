open Util

(**
   Functions on Int_ary.t, but which assume that the Int_ary.t is:
     - a set (values are unique)
     - ordered, ascending
*)

module IA = Int_ary
type t = IA.t

let t_min f ts : int =
  List.fold_left min max_int (List.map f ts)

(* !! unsafe: ts cannot be empty !! *)
let min_len : (t list -> int) =
  (* should be on regular Int_ary *)
  t_min IA.length

let min_val : (t list -> int) =
  t_min IA.hd

let max_val ts : int =
  (* unsafe_last(): ok here? *)
  List.fold_left max min_int (List.map IA.last ts)

let intersect2 (a:t) (b:t) : t =
  let res = IA.of_length (min (IA.length a) (IA.length b)) in
  let rec loop (r_off:int) (a_off:int) (b_off:int) : t =
    if a_off >= IA.length a || b_off >= IA.length b then  (* no more *)
      IA.take r_off res
    else
      let v1 = IA.unsafe_get32 a a_off
      and v2 = IA.unsafe_get32 b b_off
      in match Int32.compare v1 v2 with
	| 0 ->
	    IA.set32 res r_off v1;
	    loop (r_off+1) (a_off+1) (b_off+1)
	| 1 ->
	    loop  r_off     a_off    (b_off+1)
	| _ ->
	    loop  r_off    (a_off+1)  b_off
  in loop 0 0 0

let merge2 (a:t) (b:t) : t =
  let len_a = IA.length a and len_b = IA.length b in
  let res = IA.of_length (len_a + len_b) in
  let rec loop (r_off:int) (a_off:int) (b_off:int) =
    if a_off >= len_a then
      IA.append b b_off res r_off
    else
      if b_off >= len_b then
	IA.append a a_off res r_off
      else
	let v1 = IA.unsafe_get32 a a_off
	and v2 = IA.unsafe_get32 b b_off
	in match Int32.compare v1 v2 with
	  | 0 ->
	      IA.set32 res r_off v1;
	      loop (r_off+1) (a_off+1) (b_off+1)
	  | 1 ->
	      IA.set32 res r_off v2;
	      loop (r_off+1)  a_off    (b_off+1)
	  | _ ->
	      IA.set32 res r_off v1;
	      loop (r_off+1) (a_off+1)  b_off
  in loop 0 0 0

let list_op f = function
  | []      -> IA.empty
  | t :: ts -> List.fold_left f t ts

let merge ts : t = list_op merge2 ts

(* Put shortest lists first.
   Their intersections are more likely to produce null set.
   With null set, we can short-circuit. *)
let intersect ts : t =
  let lists_short_to_long =
    List.fast_sort (fun a b -> IA.length a <=> IA.length b) ts
  in list_op intersect2 lists_short_to_long
(*
  Attempt imperative solution, with in-place update.
  Take shortest first.
  Except that this fun is never actually called anymore!
  Instead, we intersect lazily, using iter_funs, always taking some limit.
  (We *could* use it to find charities results before filtering by in-stock.)
*)


(* INclusive *)
let values_in t (lo:int) (hi:int) : t =
  let high_enough i = i >= lo  (* pt-free too confusing *)
  and too_high i = i > hi
  in match IA.index_where high_enough t with
    | None -> IA.empty
    | Some left ->
	let right = match IA.index_where too_high t with
	  | None -> IA.length t
	  | Some i -> i
	in IA.unsafe_sub t left (right-left)

(*
  Not functional; holds state:
    - v : last-accessed value
    - i : index of v

  Index (i) will not advance unnecessarily.
  Keep iterating until find a value large enough
  (or raise invalid_arg exception).
*)
let iter_fun t : Iter_fun.t =
  let max_val = IA.last t
  and v = ref min_int    (* last value: cache, because that's faster. *)
  and i = ref (-1)       (* index: where in (t) we've moved to so far. *)
  in
    fun (min_val:int) ->
      if min_val > max_val then  (* can't get there *)
	raise Not_found
      else (
	(*
	  Rather than simply incrementing here,
	  we could attempt some cleverness to find min_val sooner.
	  Perhaps a binary chop, since we can easily know:
	  - the max value
	  - the number of values
	*)
	while !v < min_val do
	  incr i;
	  v := IA.unsafe_get t !i  (* should never raise. *)
	done;
	!v
      )

(*
  IF WE ASSUME AN EVEN DISTRIBUTION OF VALUES...

  Alternately, do a simple binary chop, but just use an INITIAL
  midpoint that's closer to the min than to the max.
*)
(***
let next_idx_and_value_gte t (i:int) (v:int) (min_val:int) : (int * int) =
  let max_val = IA.last t in
    if min_val > max_val then  (* can't get there *)
      raise Not_found
    else
      (* try... *)
      let rec loop i' v' =
	(* already there *)
	if v' >= min_val then
	  (i',v')
	else
	  (* where should we try? *)
	  let guess_i =
	    let desired_run =
	      let desired_rise = min_val - v'
	      and ave_rise = (* rise/run.  just int. *)
		(max_val - v') / (IA.length t - i')
	      in
		desired_rise / ave_rise
	    in i' + desired_run
	  in
	    (* look at value at guess_i *)
	  let guess_v = IA.get t guess_i
	    (* if val is too low, then loop *)
	    if guess_v 
	    (* if val is too HIGH, then what? *)
*)

