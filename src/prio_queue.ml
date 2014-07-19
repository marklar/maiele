module type ORDERED_TYPE = sig
  type t
  val cmp : t -> t -> bool (* (>=) : max at top.  (<=) : min at top  *)
end

module Make =
  functor (Priority:ORDERED_TYPE) -> struct

    type priority = Priority.t
	
    (* 'a: element *)
    type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

    let empty = Empty

    let rec insert t (prio:priority) (elt:'a) : 'a t =
      match t with
	| Empty -> Node (prio, elt, Empty, Empty)
	| Node (p, e, left, right) ->
	    (* to keep balanced, keep switching sub-trees.
	       insert at head of R, and swap it to L. *)
            if Priority.cmp prio p
	    then Node (prio, elt, insert right p e,      left)
            else Node (p,    e,   insert right prio elt, left)
      
    exception Is_empty

    let rec remove_top_exn t : 'a t =
      match t with
	| Empty -> raise Is_empty
	| Node (_, _, left,  Empty) -> left
	| Node (_, _, Empty, right) -> right
	| Node (_, _,
		(Node (lp, le, _, _) as left),
		(Node (rp, re, _, _) as right)
	       ) ->
	    (* put as head the lower of subtree heads.
	       keep left and right in-place. *)
	    if Priority.cmp lp rp
	    then Node (lp, le, remove_top_exn left, right)
	    else Node (rp, re, left,                remove_top_exn right)
	      
    (* If you also want the top value. *)
    let extract_exn t : (priority * 'a * 'a t) =
      match t with
	| Empty -> raise Is_empty
	| Node (p, e, _, _) as t -> (p, e, remove_top_exn t)
	    
    let peek_exn : 'a t -> priority * 'a = function
      | Empty -> raise Is_empty
      | Node (p, e, _, _) -> (p, e)

    let maybe_insert_keeping_size_exn t (prio:priority) (elt:'a) : 'a t =
      match t with 
	| Empty -> raise Is_empty
	| Node (p, e, _, _) ->
	    if not (Priority.cmp prio p)
	    then insert (remove_top_exn t) prio elt
	    else t
	  
    let to_list t : 'a list =
      let rec loop acc t' =
	try let (_, e, t'') = extract_exn t' in loop (e::acc) t''
	with Is_empty -> acc
      in loop [] t

  end
