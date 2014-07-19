type t = int -> int

(*
  Why raise Not_found (or invalid_arg, in case of Ord_int_ary.iter_fun),
  rather than returning an int option?  Faster.

  Using an option return...
    + More clearly expresses semantics
    + Flow of control is more obvious.
    - Makes the code a touch more cumbersome.
    - Most importantly, it's slower.  Speed is really important here.
*)
let failed_fun_exn : t = fun _ -> raise Not_found

(*
  Each of these functions *creates* a function.
*)

(*
  Remember last value returned.
  When asked for next, call iter_fun with a min of last value + 1.
*)
let next_val_fun (t:t) : unit -> int =
  let pv = ref min_int in  (* previous value *)
    fun () ->
      pv := t (!pv + 1);   (* get next value > previous *)
      !pv

(*
  The funs created below don't hold their own state.
  We pass in the minimum value (min_v) we'll accept back,
  which they then use to ask their "children" for the same.

  The leaf nodes in this function tree wrap Ord_int_arys.
  Those functions do remember "where they are" (i.e. the
  index of the last value asked of them.)
*)

(*
  Each time created fun is called, it:
    - returns the next value its two args have in common, or
    - if one is exhausted, raises Not_found
*)
let and_fun_exn (next_l:t) (next_r:t) : t =
  fun (min_v:int) ->
    let rec loop lv rv = match compare lv rv with
      |  0 -> lv
      | -1 -> loop (next_l rv) rv    (* may raise *)
      |  _ -> loop lv (next_r lv) in (* may raise *)
    let lv = next_l min_v in
      loop lv (next_r lv)

(*
  Each time created fun is called, it:
    - returns the lower of the next values from its two args, or
    - if they're both exhausted, raises Not_found
*)
let or_fun_exn (next_l:t) (next_r:t) : t =
  fun (min_v:int) ->
    let lv = (try Some (next_l min_v) with _ -> None)
    and rv = (try Some (next_r min_v) with _ -> None)
    in match (lv, rv) with
      | (None, None)                    -> raise Not_found
      | (Some v, None) | (None, Some v) -> v
      | (Some a, Some b)                -> min a b

(*
  Each time the created fun is called, it:
    - gets next (internal) value (if any) at least as large as min_v.
      + if it IS min_v, then we don't want that.  try again.
      + if it's LARGER THAN min_v, or there's no next value,
        then we want min_v.
*)
let not_fun (next:t) : t =
  let rec f (min_v:int) =
    let next_val = 
      try Some (next min_v)
      with _ -> None
    in match next_val with
      | Some v when v = min_v -> f (min_v+1)
      | _ -> min_v
  in f
