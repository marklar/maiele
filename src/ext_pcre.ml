(*
  Meant to be an extension to the standard Pcre module.

  But replace_hexes(), though very useful, and certainly using regexps,
  is out of place here.

  What these *really* are are utility funs for logfile analysis.
  It'd be worth renaming this module.
*)

open Pcre

let hex_2_regular : (Pcre.regexp * Pcre.substitution) list =
  List.map
    (fun (pat, sub) -> ((regexp pat), (subst sub)))
    [ ("%20", " ")
    ; ("%23", "#")
    ; ("%26", "&")
    ; ("%27", "'")
    ; ("%2F", "/")
    ; ("%3D", "=")
    ; ("%3F", "?")
    ; ("%5B", "[")
    ; ("%5C", "\\")
    ; ("%5D", "]")
    ; ("%5E", "^")
    ; ("%7B", "{")
    ; ("%7C", "|")
    ; ("%7D", "}")
    ; ("%25", "%")  (* important: put last, because '%' is in patterns. *)
    ]

(*
  >> replace_hexes
      "http%3A%2F%2Fwww.gamespot.com%2Fxbox360%2Fstrategy%2Fmonopoly%2Findex.html%3Fv=1"
  => "http://www.gamespot.com/xbox360/strategy/monopoly/index.html?v=1"
*)
let replace_hexes (s:string) : string =
  List.fold_left
    (fun s (rex, itempl) -> replace s ~rex ~itempl)
    s
    hex_2_regular

let matches_from_strs (rex:regexp) (lines:string list) : string array list =
  let rec loop acc = function
    | []    -> acc
    | l::ls ->
	let acc' =
	  try extract l ~rex :: acc
	  with Not_found -> acc
	in loop acc' ls
  in List.rev (loop [] lines)

let matches_from_strs_enum (rex:regexp) (lines:string Enum.t) : string array Enum.t =
  (*
    http://ocaml-extlib.googlecode.com/svn/doc/apiref/Enum.html#TYPEt
    filter_map : ('a -> 'b option) -> 'a t -> 'b t
    filter_map f e
      -> enumeration over all elements x for which f y returns Some x,
         where y is an element of e.
  *)
  Enum.filter_map
    (fun s -> try Some (extract s ~rex) with Not_found -> None)
    lines

