open Util;;  open Printf

type t =
    { offs : Int_ary.t
    ; data : Int_ary.t
    ; len  : int        (* # records.  rename? *)
    }

(* Too much reduplicated logic.  Create a module offset_tbl. *)

let unsafe_offset t (id:int) : int =
  Int_ary.get t.offs (id-1)

let int32_bytes = 4

let idx_and_len t (id:int) : int * int =
  let num_ints i = i / int32_bytes in
  let off = num_ints $ unsafe_offset t id
  and aft =
    if id = t.len then
      Int_ary.length t.data
    else
      num_ints $ unsafe_offset t (id+1)
  in
    (off, aft-off)
	  
(*-- public --*)

let open_tbl (dir:string) (root:string) : t =
  let (offs, data) = 
    let tbl ext = Int_ary.from_file dir (root ^ ext) in
    let d =
      try tbl ".data"  (* mmap fails if file size = 0. *)
      with _ -> Int_ary.empty
    in (tbl ".offs", d)
  in
    { offs = offs
    ; data = data
    ; len = Int_ary.length offs
    }

let close t : unit =
  List.iter Int_ary.close [t.offs; t.data]

let length t = t.len

let unsafe_get t (id:int) : Int_ary.t =
  let (idx, len) = idx_and_len t id in
    Int_ary.sub_exn t.data idx len

let iter (f:Int_ary.t -> 'a) t : unit =
  for i = 1 to t.len do
    f (unsafe_get t i)
  done

let iteri (f:int -> Int_ary.t -> 'a) t : unit =
  for i = 1 to t.len do
    f i (unsafe_get t i)
  done

(*
  Return val may be Int_ary.empty.
  May raise.
*)
let get_exn t (id:int) : Int_ary.t =
  if id <-> (1, t.len) then
    unsafe_get t id
  else
    invalid_arg $ sprintf "Int_ary_tbl.get_exn: index out of bounds: %d" id

(*
  Impl'd like fold_left, but w/ *2* funcs!
    - extreme_f : fun used to choose extreme value.
    - init      : initial value.
    - extract_f : fun used to fetch val from t.  
*)
let extreme_val t (extreme_f:int -> int -> int) (init:int)
    (extract_f:Int_ary.t -> int) : int =
  let r = ref init in
    for i = 1 to t.len do
      let vals = unsafe_get t i in
	if not (Int_ary.empty_p vals) then
	  r := extreme_f !r (extract_f vals)
    done;
    !r

let min_val t : int = extreme_val t min max_int Int_ary.hd
let max_val t : int = extreme_val t max 0       Int_ary.last
