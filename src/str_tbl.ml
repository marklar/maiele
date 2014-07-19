open Printf;;  open Util

type t = { offs : Int_ary.t
	 ; data : Char_ary.t
	 ; len  : int  (* # strings *)
	 }

let unsafe_offset t (id:int) : int =
  Int_ary.get t.offs (id-1)

let off_and_len t (id:int) : int * int =
  let off = unsafe_offset t id
  and aft =
    if id = t.len then
      Char_ary.length t.data
    else
      unsafe_offset t (id+1)
  in (off, aft-off)

(*--- public ---*)

let open_tbl (dir:string) (file_root:string) : t =
  let (offs, data) =
    let fn kind = file_root ^ "." ^ kind in
    let o = Int_ary.from_file  dir (fn "offs")
    and d = Char_ary.from_file dir (fn "data")
    in (o,d)
  in
    { offs = offs
    ; data = data
    ; len  = Int_ary.length offs
    }

let close t : unit =
  Int_ary.close  t.offs;
  Char_ary.close t.data

let length t = t.len

let get_exn t (id:int) =
  if id <-> (1, t.len) then
    let (off, len) = off_and_len t id in
      Char_ary.unsafe_get_str t.data off len
  else
    invalid_arg (sprintf "Str_tbl.get_exn: index out of bounds: %d" id)

let iter (f:string -> 'a) t : unit =
  for i = 1 to t.len do
    f (get_exn t i)
  done

let iteri (f:int -> string -> 'a) t : unit =
  for i = 1 to t.len do
    f i (get_exn t i)
  done
