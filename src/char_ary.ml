open Bigarray;;  open Util

type t =
    { ary  : (char, int8_unsigned_elt, c_layout) Array1.t
	(* Must keep ary, else GC bojacks you. *)
    ; data : string
    ; len  : int
    ; fd   : Unix.file_descr
    }

let length t = t.len

let from_file ?(shared=false) (dir_name:string) (file_name:string) : t =
  let fd = Mmap.file_descr dir_name file_name shared in
  let ary = Array1.map_file fd char c_layout shared (-1) in
    { ary  = ary
    ; data = (Obj.magic (Obj.field (Obj.repr ary) 1) : string)
    ; len  = Array1.dim ary
    ; fd   = fd
    }

let close t : unit = Unix.close t.fd

(* just one char *)

let unsafe_get t n = String.unsafe_get t.data n
let unsafe_set t n c = String.unsafe_set t.data n c

let get_exn t n =
  if n <-> (1, t.len-1) then
    unsafe_get t n
  else
    invalid_arg "Char_ary.get: index out of bounds"

let set_exn t n c =
  if n <-> (1, t.len-1) then
    unsafe_set t n c
  else
    invalid_arg "Char_ary.set: index out of bounds"

(*
  Gets a string.
  Can't use blit.  Won't return actual string.
*)
let unsafe_get_str t off len =
  let str = String.create len in
    for i = 0 to len-1 do
      str.[i] <- unsafe_get t (off + i)
    done;
    str

