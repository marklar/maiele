open Bigarray;;  open Util;;  open Printf;;  open ExtList

(* type order = Asc | Desc *)

(* Store data in a one-dimensional array.  Store its length. *)
type ary_t = (int32, int32_elt, c_layout) Array1.t
type t =
    { ary : ary_t
    ; len : int   (* in ints *)
    ; fd  : Unix.file_descr option
    }
    
let close t : unit = match t.fd with
  | Some fd -> Unix.close fd
  | None -> ()  (* raise exception here? *)

let from_file ?(shared=false) (dir_name:string) (file_name:string) : t =
  let fd = Mmap.file_descr dir_name file_name shared in
  let ary = Array1.map_file fd int32 c_layout shared (-1) in
    { ary = ary
    ; len = Array1.dim ary   (* dim(): bad name.  better: len(). *)
    ; fd  = Some fd
    }   

let of_int32_array (in_ary:int32 array) : t =
  { ary = Array1.of_array Bigarray.int32 c_layout in_ary
  ; len = Array.length in_ary
  ; fd  = None
  }
    
let empty : t = of_int32_array [||]

let of_array (in_ary:int array) : t =
  let int32_ary = Array.map Int32.of_int in_ary in
    of_int32_array int32_ary

let of_int32_list (l:int32 list) : t =
  of_int32_array (Array.of_list l)
    
let of_list (l:int list) : t =
  of_int32_list (List.map Int32.of_int l)
    
let of_length (len:int) : t =
  { ary = Array1.create Bigarray.int32 c_layout len
  ; len = len
  ; fd  = None
  }

let take (num:int) t : t =
  { ary = t.ary
  ; len = min num t.len
  ; fd  = None
  }

let unsafe_sub t (idx:int) (len:int) : t =
  { ary = Array1.sub t.ary idx len
  ; len = len
  ; fd  = None
  }
  
let sub_exn t (idx:int) (len:int) : t =
  if idx + len > Array1.dim t.ary then
    invalid_arg $ sprintf "Int_ary.sub_exn. idx, len: %d, %d" idx len
  else
    unsafe_sub t idx len

let length  t : int  = t.len
let empty_p t : bool = (t.len = 0)

let eql t1 t2 : bool =
  if t1.len <> t2.len then
    false
  else
    let rec loop i =
      if i = t1.len then
	true
      else begin
	if t1.ary.{i} <> t2.ary.{i} then
	  false
	else
	  loop (i+1)
      end
    in loop 0

let unsafe_get32 t (i:int) : int32 = t.ary.{i}
let unsafe_get t (i:int) : int = Int32.to_int t.ary.{i}

let to_array t : int array =
  Array.init t.len (fun i -> unsafe_get t i)
  
(* .{i}: Bigarray.Array1.t index syntax. *)
let get32 t (i:int) : int32 =
  if i <-> (0, t.len-1) then
    unsafe_get32 t i
  else
    invalid_arg $ sprintf "Int_ary.get32: %d" i

let get t (i:int) : int =
  if i <-> (0, t.len-1) then
    unsafe_get t i
  else
    invalid_arg $ sprintf "Int_ary.get: %d" i
  
(*
  PRIVATE, as it doesn't conform to external expectations:
  (Array1.length dst.ary) > dst.len :
  known to be enough to support appending src.
*)
let append (src:t) (src_o:int) (dst:t) (dst_o:int) : t =
  let len = src.len - src_o in
  let src1 = unsafe_sub src src_o len
  and dst1 = unsafe_sub dst dst_o len
  in
    Array1.blit src1.ary dst1.ary;
    { ary = dst.ary
    ; len = dst_o + len
    ; fd  = None
    }

let set32 t (idx:int) (v:int32) : unit = t.ary.{idx} <- v
let set   t (idx:int) (v:int)   : unit = set32 t idx (Int32.of_int v)
let safe_set t (idx:int) (v:int) : unit =
  if idx <-> (0, t.len-1) then
    set t idx v
  else
    invalid_arg $ sprintf "Int_ary.safe_set: %d" idx

(* fix: -> int option ?  raise exception?
 * why is it that we haven't run into a prob with this? *)
let hd t : int = unsafe_get t 0
let tl t : t   = unsafe_sub t 1 (t.len-1)

let unsafe_last t : int = unsafe_get t (t.len-1)
let last t : int = get t (t.len-1)

(* Cannot use 'match' here, as '::' unsupported. *)
let rec find (f:int -> bool) t : int option =
  if empty_p t then
    None
  else
    let h = hd t in
      if f h then
	Some h
      else
	find f (tl t)

let iter (f:int -> 'a) t : unit =
  for i = 0 to t.len-1 do
    f (unsafe_get t i)
  done

let map2 (f:int -> int -> 'a) (a:t) (b:t) : 'a list =
  let rec loop acc i =
    if i >= a.len || i >= b.len then
      acc
    else
      loop (f (get a i) (get b i) :: acc) (i+1)
  in List.rev (loop [] 0)

(*
  Analogous to ExtList.List.combine
  In Haskell, this would be called 'zip'.
*)
let combine (a:t) (b:t) : (int * int) list =
  map2 (fun x y -> (x,y)) a b

(* funarg's args: [1] idx, [2] elt *)
let iteri (f:int -> int -> 'a) t : unit =
  for i = 0 to t.len-1 do
    f i (unsafe_get t i)
  done

let fold_left (f:'a -> int -> 'a) (init:'a) t : 'a =
  let memo = ref init in
    iter (fun i -> memo := f !memo i) t;
    !memo

(* for testing *)
let is_ordered t : bool =
  let rec loop idx prev =
    if idx >= t.len then
      true
    else
      let curr = get t idx in
	match prev with
	  | None -> loop (idx+1) (Some curr)
	  | Some pv ->
	      if curr < pv then
		false
	      else
		loop (idx+1) (Some curr)
  in loop 0 None

let index_where (f:int -> bool) t : int option =
  let rec loop idx =
    if idx >= t.len then
      None
    else
      if f (unsafe_get t idx) then
	Some idx
      else
	loop (idx+1)
  in loop 0

let index t (v:int) : int option =
  index_where ((=) v) t

(*
  Short-circuits upon false.
  More efficient to use an iterator than tl?
  Cannot use 'match' here, as '::' unsupported.
*)
let rec for_all (f:int -> bool) t : bool =
  empty_p t ||
    (f (hd t) && for_all f (tl t))

let rec exists (f:int -> bool) t : bool =
  if empty_p t then
    false
  else
    f (hd t) || exists f (tl t)

(* N.B.: returns LIST *)
let rec map (f:int -> 'a) t : 'a list =
  if empty_p t then   (* Cannot pattern match; '::' unsupported. *)
    []
  else
    f (hd t) :: map f (tl t)

(* N.B.: returns LIST *)
let rec filter (f:int -> bool) t : int list =
  if empty_p t then
    []
  else 
    let h = hd t in
      if f h then
	h :: filter f (tl t)
      else
	filter f (tl t)
    
let rec to_list t : int list =
  map id t

(* for internal use *)
let to_s t (pre:string) (sep:string) (post:string) : string =
  pre ^ (String.concat sep (map string_of_int t)) ^ post

let to_json_ary t : string = to_s t "["   ","   "]"
let show        t : string = to_s t "[|"  "; "  "|]"

(* if UN-ordered *)
let max_val_unordered t : int =
  let max_val = ref min_int in
  let maybe_update_max (i:int) : unit =
    if i > !max_val then
      max_val := i
  in
    iter maybe_update_max t;
    !max_val

let concat ts : t =
  (* create ia -- big enough to hold all ts end-to-end. *)
  let res =
    let sum_of_lens = List.fold_left (+) 0 (List.map length ts)
    in of_length sum_of_lens
  in
    (* update 'res' in place *)
  let rec blit_multi (res_off:int) : (t list -> unit) = function
    | [] -> ()
    | hd :: tl ->
	Array1.blit
	  (Array1.sub hd.ary 0 hd.len)
	  (Array1.sub res.ary res_off hd.len);
	blit_multi (res_off + hd.len) tl
  in
    blit_multi 0 ts;
    res
