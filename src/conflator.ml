(**
   treat different lexemes as same.
   List.iter print_endline (conflations "there")
*)  

open Pcre;; open Util
 
let in_file_name = "./cfg/conflations.txt"

(* reversed.  but no matter. *)
let lines_from_file (file_name:string) : string list =
  let in_chan = open_in file_name in
  let rec loop acc =
    try
      let ln = input_line in_chan
      in loop (ln :: acc)
    with End_of_file ->
      close_in in_chan;
      acc
  in loop []
       

let like_strings_from_file (file_name:string) : string list list =
  List.map
    (split |> String.lowercase)
    (lines_from_file file_name)

type pair = (string * string)

(*
  ["xi"; "11"; "eleven"]  -> 

  [ ("xi", "11")
  ; ("xi", "eleven")
  ; ("11", "eleven")
  ]
*)
let pairs (strs:string list) : pair list =
  let rec loop acc = function
    | [] | _ :: [] -> acc
    | hd :: tl ->
	let ps = List.map (fun s -> (hd, s)) tl in
	  loop (ps @ acc) tl
  in loop [] strs

(*
  ["xi"; "11"; "eleven"]  -> 

  [ ("xi",     "11")
  ; ("xi",     "eleven")
  ; ("11",     "eleven")
  ; ("11",     "xi")
  ; ("eleven", "xi")
  ; ("eleven", "11")
  ]
*)
let bipolar_pairs (strs:string list) : pair list =
  let ps = pairs (Util.uniq strs)
  and flip p = (snd p, fst p) in
    ps @ (List.map flip ps)

(*
  For list of conflations (e.g. ["xi"; "11"; "eleven"]),
  add all possible pairings to hashmap.
*)
let add_pairings (h:(string,string) Hashtbl.t) (strs:string list) : unit =
  let h_add (k,v) = Hashtbl.add h k v in
    List.iter h_add (bipolar_pairs strs)

let dict : (string,string) Hashtbl.t =
  let h = Hashtbl.create 128 in
    List.iter (add_pairings h) (like_strings_from_file in_file_name);
    h

(*-- public --*)

let conflations (lexeme:string) : string list =
  Hashtbl.find_all dict lexeme
