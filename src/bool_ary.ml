open Bigarray

type t = { ary : (int, int8_unsigned_elt, c_layout) Array1.t
	 ; len : int
	 ; fd  : Unix.file_descr
	 }

let from_file ?(shared=false) (dir_name:string) (file_name:string) : t =
  let fd = Mmap.file_descr dir_name file_name shared in
  let ary = Array1.map_file fd int8_unsigned c_layout shared (-1) in
    { ary = ary
    ; len = Array1.dim ary
    ; fd  = fd
    }

let close t : unit = Unix.close t.fd
let length t : int = t.len

let get_int t (idx:int) : int =
  Array1.get t.ary idx

let get t (idx:int) : bool =
  (get_int t idx) <> 0

let set t (idx:int) (v:bool) : unit =
  Array1.set t.ary idx (if v then 1 else 0)