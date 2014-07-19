open Printf

(* Minimum.
 * Deliberately monomorphic, as OCaml does not perform
 * type specialization for user-defined functions.
 *)
let min3 (x:int) (y:int) (z:int) : int =
  let m' (a:int) b : int = if a < b then a else b in
    m' (m' x y) z

(*
  array (length m) of arrays (each of length n).
  ary 0, each element is set to its index.
  other arrays:
    - first element: array num
    - other elements: 0
*)      
let init_matrix (m:int) (n:int) =
  Array.init m (function
		  | 0 -> Array.init n (function j -> j)
		  | i -> Array.init n (function 0 -> i | _ -> 0)
	       )

 
(* Computes the Levenshtein distance between two unistrings.
 * If you want to run it faster, add the -unsafe option when
 * compiling or use Array.unsafe_* functions (but be careful
 * with these well-named unsafe features). *)
let distance_utf8 (x:int array) (y:int array) : int =
  match (Array.length x, Array.length y) with
    | (0, n) -> n
    | (m, 0) -> m
    | (m, n) ->
	let matrix = init_matrix (m + 1) (n + 1) in
	  for i = 1 to m do  (* for each char in x *)
	    let s = matrix.(i) and t = matrix.(i - 1) in
	      for j = 1 to n do  (* for each char in y *)
		let cost = abs (compare x.(i - 1) y.(j - 1)) in
		  s.(j) <- min3 (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
	      done
	  done;
	  matrix.(m).(n)

let to_int_array (s:string) : int array =
  let len = String.length s in
  let us = Array.create len 0 in
    for i = 0 to len - 1 do
      us.(i) <- Char.code s.[i]
    done;
    us

(* This function takes two strings, convert them to unistring (int array)
 * and then call distance_utf8, so we can compare utf8 strings. Please
 * note that you need Glib (see LablGTK). *)
let distance (x:string) (y:string) : int =
  distance_utf8 (to_int_array x) (to_int_array y)
  (* distance_utf8 (Glib.Utf8.to_unistring x) (Glib.Utf8.to_unistring y) *)

let score (x:string) (y:string) : float =
  1. /. (float_of_int (distance x y))

(*
let _ =
  let score =
    Util.time_it "levenshtein" (fun () -> distance (Sys.argv.(1)) (Sys.argv.(2)))
  in printf "score: %d\n" score
*)
