open Printf;;  open Pcre

let amp_re  = regexp "\\&\\s"
let amp_sub = subst  "&amp; "
let entitize_amps (s:string) : string =
  replace s ~rex:amp_re ~itempl:amp_sub

let maybe_wrap_with_name (format:Request.format) (name:string option)
    (s:string) : string =
  match name with
    | None   -> s
    | Some n -> match format with
	| `Json -> sprintf "\"%s\":%s" n s
	| `Xml  -> sprintf "<%s>\n%s\n</%s>\n" n s n
	| `Html -> sprintf "%s\n<ul>\n%s\n</ul>\n" n s

(* creates a JSON obj literal *)
let jsonize_pairs (pairs:(string * string) list) : string =
  let str = 
    let strs = List.map (fun (k,v) -> sprintf "\"%s\":%s" k v) pairs
    in String.concat ", " strs
  in sprintf "{%s}" str  (* no need to wrap *)

(* *)
let xmlize_pairs (name:string option) (pairs:(string * string) list) : string =
  let str = 
    let strs = List.map (fun (k,v) -> sprintf "<%s>%s</%s>" k v k) pairs
    in String.concat "\n" strs
  in entitize_amps (maybe_wrap_with_name `Xml name str)

let htmlize_pairs (name:string option) (pairs:(string * string) list) : string =
  let str =
    let strs = List.map (fun (k,v) -> sprintf "<li>%s: %s</li>" k v) pairs
    in String.concat "\n" strs
  in maybe_wrap_with_name `Html name str

let show_pairs (format:Request.format) (name:string option)
    (pairs:(string * string) list) : string =
  match format with
    | `Json -> jsonize_pairs pairs
    | `Html  (* to do *)
    | `Xml  -> xmlize_pairs name pairs

let show_list (format:Request.format) (elem_strs:string list) : string =
  match format with
    | `Json -> sprintf "[%s]" (String.concat ",\n" elem_strs)
    | `Html  (* to do *)
    | `Xml  -> String.concat "\n" elem_strs
