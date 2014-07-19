open ExtList
open Util

type t = { head    : Int_ary.t
	 ; lexemes : Str_tbl.t
	 ; len     : int
	 }

let first_id_for_head t (ch_code:int) : int =
  Int_ary.get t.head ch_code

let unsafe_get t (id:int) : string =
  Str_tbl.get_exn t.lexemes id  

let max_ch_code = 255
let rec id_after t (ch_code:int) : int =
  if ch_code >= max_ch_code then
    t.len+1
  else
    let next_ch_code = ch_code + 1 in
      match first_id_for_head t next_ch_code with
	| 0  -> id_after t next_ch_code
	| id -> id

(* public: for denormalizer *)
let bound_ids t (ch_code:int) : (int * int) option =
  match first_id_for_head t ch_code with
    | 0  -> None
    | id -> Some (id, id_after t ch_code)

type nearest_id = At of int | After of int

(* private *)
let rec bsearch_id t (lxm:string) (min:int) (max:int) : nearest_id =
  if min > max then
    After min
  else
    let mid = (min + max) / 2 in
      match compare lxm (unsafe_get t mid) with
	| 0 -> At mid
	| 1 -> bsearch_id t lxm (mid+1) max
	| _ -> bsearch_id t lxm min     (mid-1)

type lexeme_id_match =
  | Miss
  | Complete of int
  | Partial of (int * int)
			 
let find_id t (lxm:string) : lexeme_id_match =
  match bound_ids t (Char.code lxm.[0]) with
    | None -> Miss
    | Some (beg, aft) ->
	if lxm = unsafe_get t beg then
	  Complete beg  (* optimize; 1-ch lexemes *)
	else
	  match bsearch_id t lxm beg aft with
	    | After id -> Partial (id, aft)
	    | At id    -> Complete id
		
(*--- public ---*)

(* return list:
     - may include first_id.
     - rev-sorted, 'cuz it doesn't matter.
   public, for benefit of denormalize_mtx. *)
let shortest_super_ids t (lxm:string) (first_id:int) (after_id:int) : int list =
  let rec loop (acc:int list) (id:int) (prev_lxm:string option) : int list =
    if id >= after_id then
      acc
    else
      let lxm' = unsafe_get t id in
	if not (superstr_p lxm' lxm) then
	  acc
	else
	  let acc' = match prev_lxm with
	    | Some p when superstr_p lxm' p -> acc  (* lxm' not among shortest *)
	    | _ -> id :: acc
	  in loop acc' (id+1) (Some lxm')
  in loop [] first_id None  (* rev unnecessary *)

(*
  All lexemes chosen must be superstring of 'lxm'.
  Take only 'n' matches.
*)
let next_n_matching_ids t (n:int) (lxm:string) (first_id:int) (after_id:int)
    : int list =
  let rec loop (n':int) (acc:int list) (id:int) : int list =
    if n' <= 0 || id >= after_id then
      acc
    else
      let lxm' = unsafe_get t id in
	if not (superstr_p lxm' lxm) then
	  acc
	else
	  loop (n'-1) (id :: acc) (id+1)
  in List.rev (loop n [] first_id)
	  
let open_tbl (dir:string) (root:string) : t =
  let head = Int_ary.from_file dir (root ^ ".hd:id.data")
  and lexemes = Str_tbl.open_tbl dir root
  in { head    = head
     ; lexemes = lexemes
     ; len     = Str_tbl.length lexemes
     }

let close t : unit =
  Int_ary.close t.head;
  Str_tbl.close t.lexemes

let length t : int = t.len

(* IDs start at 1. *)
let get_exn t (id:int) : string =
  if id <-> (1, t.len) then
    unsafe_get t id
  else
    invalid_arg "Lexicon.get_exn: index out of bounds"

(*
  Lexeme IDs for shortest superstrings of lexeme.
*)
let ids t (lexeme:string) : int list =
  match find_id t lexeme with
    | Miss               -> []
    | Complete id        -> [id]
    | Partial (beg, aft) -> shortest_super_ids t lexeme beg aft


(*--- For product codes.  (UPC/EAN) ---*)

(*
  Lexeme IDs for next N superstrings of lexeme,
  regardless of whether they're the shortest.
*)
let n_matching_ids t (n:int) (lexeme:string) : int list =
  match find_id t lexeme with
    | Miss               -> []
    | Complete id        -> [id]   (* assume it's the only match? *)
    | Partial (beg, aft) -> next_n_matching_ids t n lexeme beg aft

let complete_match_id t (lexeme:string) : int option =
  match find_id t lexeme with
    | Complete id -> Some id
    | _           -> None


(*
  Holds state:
    - previous ID
    - searched-for lexeme
*)
let next_matching_id_fun t (lexeme:string) : unit -> int option =
  let id = match find_id t lexeme with
    | Miss           -> None
    | Complete i     -> Some i
    | Partial (i, _) -> Some i
  in match id with
    | None -> (fun () -> None)
    | Some i ->
	let id_  = ref i in
	  (fun () ->
	     incr id_;
	     try
	       let lxm = get_exn t (!id_) in
		 if not (superstr_p lxm lexeme) then
		   None
		 else
		   Some (!id_)
	     with Invalid_argument _ ->
	       None )
