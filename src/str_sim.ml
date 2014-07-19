(* http://www.catalysoft.com/articles/StrikeAMatch.html *)
(*
  Have one query.
  For each considered match, compute a similarity score.
  (Precompute the bigrams for the query string.)
*)

open Printf
open Util

type bigrams = string list

let contains_space_re = Pcre.regexp " "

let bigrams_hash (bs:bigrams) : (string, int) Hashtbl.t =
  let h = Hashtbl.create 20 in
    List.iter (fun s -> Hashtbl.add h s 1) bs;
    h

(*
  Whenever a match is found, that bigram is removed from ys
  to prevent matching against the same bigram >once.
  (Otherwise, 'GGGGG' would score a perfect match against 'GG'.)
*)
let intersection_len (xs:bigrams) (ys:bigrams) : int =
  let h = bigrams_hash ys in
    List.fold_left
      ( fun m s ->
	  if Hashtbl.mem h s then
	    ( Hashtbl.remove h s;
	      m + 1 )
	  else
	    m )
      0 xs

let union_len (xs:bigrams) (ys:bigrams) : int =
  List.length xs + List.length ys

(*-- public --*)

let bigrams_for_lowercase_string (s:string) : bigrams =
  let rec loop acc off = match off with
    | -1 -> acc
    | _ -> loop (String.sub s off 2 :: acc) (off-1)
  and no_space =
    not |> Pcre.pmatch ~rex:contains_space_re
  in
    List.filter no_space (loop [] ((String.length s) - 2))

let bigrams (s:string) : bigrams =
  bigrams_for_lowercase_string (String.lowercase s)

let cmp_bigrams (xs:bigrams) (ys:bigrams) : float =
  float_of_int
    (2 * (intersection_len xs ys)) /.
    (float_of_int (union_len xs ys))
  
let cmp_strs (x:string) (y:string) : float =
  cmp_bigrams (bigrams x) (bigrams y)

let cmp_bigrams_and_string (xs:bigrams) (y:string) : float =
  cmp_bigrams xs (bigrams y)
