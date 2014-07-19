(*
  For "uniquing" Results, by their IDs and by the targets they represent.

  We put "uniquing" in quotes because it's not merely a matter of uniqueness,
  but also of not representing the same targets.

  Uses Hashtbls internally.  Mutable state, but fast.
*)
type t = { ids     : (int, int) Hashtbl.t
	 ; targets : (int, int) Hashtbl.t
	 }

(*
  We guess at how big we'd like each Hashtbl to be
  based on the number of results we intend to return.
  Should be small enough that overestimating is safe
  (and not having to re-hash makes things faster).
*)
let create (fetch_size:int) : t =
  { ids     = Hashtbl.create fetch_size
  ; targets = Hashtbl.create (fetch_size * 2)
  }

(* Declare: seen this result. *)
let flag_id t (id:int) : unit =
  Hashtbl.add t.ids id 1

(* Declare: seen this target represented by some result. *)
let flag_target t (target_id:int) : unit =
  Hashtbl.add t.targets target_id 1

let flag_targets t (target_ids:int list) : unit =
  List.iter (flag_target t) target_ids

(* Ask: has this result been seen? *)
let seen_id t (id:int) : bool =
  Hashtbl.mem t.ids id

(* Ask: has this target been seen? *)
let seen_target t (target_id:int) : bool = 
  Hashtbl.mem t.targets target_id
