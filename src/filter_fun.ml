open Util

type t = Result.t -> Result.t option

let is_faux_and_seen (r:Result.t) (u:Uniquer.t) : bool =
  Result.is_faux r && (Uniquer.seen_id u (Result.id r))

(* If bop, don't keep. *)
let faux_result (r:Result.t) (u:Uniquer.t) (bop_p:bool) : Result.t option =
  if bop_p then
    None
  else (
    Uniquer.flag_id u (Result.id r);   (* flag just IDs, not targets. *)
    Some r
  )


(* Keep iff has novel targets. *)
let vrai_result (r:Result.t) (u:Uniquer.t) : Result.t option =
  let novel_target_ids : int list =
    Int_ary.filter
      (not |> Uniquer.seen_target u)
      (Result.target_ids r)
  in
    match novel_target_ids with
      | [] ->
	  None
      | ids ->
	  Uniquer.flag_targets u ids;
	  Some r
  
(*
  Creates function which takes a result ID and:
    - skips that result (None)   -OR-
    - keeps it (Some r)
*)
let create (opts:Query_opts.t) : t =
  let uniquer = Uniquer.create (Query_opts.fetch_size opts) in
  let f (result:Result.t) : Result.t option =
    (* Faux result: dupe? *)
    if is_faux_and_seen result uniquer then
      None
    else
      (* If 'browse', are there enough Glus to show? *)
      if Result.too_few_glus_p result then
	None
      else
	(* Faux v. Vrai *)
	if Result.is_faux result then
	  faux_result result uniquer (Query_opts.bop_p opts)
	else
	  vrai_result result uniquer
  in f
