open Http_types;;  open Pcre;;  open Util

(*
  Convert strings: UTF-8 <-> ISO-8859-1.

  LSD accepts requests and provides results in utf8.
  Internally, LSD uses only latin1.

  So, on the way in, convert to_latin1,
  and on the way out, convert to utf8.
*)
let cc src trg (s:string) = Netconversion.convert ~in_enc:src ~out_enc:trg s
let to_utf8   : string -> string = cc `Enc_iso88591 `Enc_utf8
let to_latin1 : string -> string = cc `Enc_utf8     `Enc_iso88591

exception Invalid_param of string * string
exception Http_method_unsupported of string
exception Invalid_path of string

type format = [ `Json |  `Xml | `Html ]

let default_format = `Json
let extension_re = regexp "\\.(html?|xml|js(on)?)"
let format (req:request) : format =
  try
    (* Pcre.extract -> [matches] *)
    match extract ~rex:extension_re ~full_match:false req#path with
      | [||] -> default_format
      | ary  -> match ary.(0) with
	  | "json" | "js"  -> `Json
	  | "xml"          -> `Xml
	  | "html" | "htm" -> `Html
	  | s -> raise $ Invalid_path req#path
  with Not_found -> default_format (* why would this happen? *)

let str_param (req:request) (name:string) (default:string) (range:string list)
    : string =
  let str = req#param ~default name in
    if not $ List.mem str range then
      raise $ Invalid_param (name, str)
    else
      str

let comma_re = regexp ","
let str_list_param (req:request) (name:string) (default:string list)
    (range:string list) : string list =
  let strs =
    let req_str =
      req#param name ~default:(String.concat "," default)
    in split req_str ~rex:comma_re
  in
    List.iter
      (fun s ->
	 if not (List.mem s range) then
	   raise $ Invalid_param (name, s)
      )
      strs;
    strs

let int_param (req:request) (name:string) : int =
  int_of_string $ req#param name

let float_param (req:request) (name:string) : float =
  float_of_string $ req#param name

let bool_param (req:request) (name:string) : bool =
  match req#param ~default:"false" name with
    | "1" -> true
    | "0" -> false
    | s -> bool_of_string s

let query_str (req:request) : string =
  (* Comes in as UTF-8. *)
  match to_latin1 $ req#param ~default:"" "query" with
    | ""  -> raise $ Invalid_param ("query", "")
    | str -> str

let upper_and_lower_domain_names:string list = 
  let ns = Cfg.all_domain_names in
    (List.map String.lowercase ns) @ (List.map String.capitalize ns)

let domain_name (req:request) (default:string) : string =
  str_param req "domain" default upper_and_lower_domain_names

(* support either 'domain' or 'domains' *)
let domain_names (req:request) (default:string list) : string list =
  let name =
    try ignore (req#param "domain"); "domain"
    with _ -> "domains"
  in str_list_param req name default ("all" :: upper_and_lower_domain_names)

let validated_int (name:string) (req:request) (default:int)
    (validate:int -> bool) : int =
  let str = req#param ~default:(string_of_int default) name in
  let res = int_of_string str in
    if not (validate res) then
      raise $ Invalid_param (name, str)
    else
      res

let non_negative_int (name:string) (req:request) (default:int) : int =
  validated_int name req default (fun i -> i >= 0)

let positive_int (name:string) (req:request) (default:int) : int =
  validated_int name req default (fun i -> i > 0)

let show_size (req:request) (default:int) : int =
  positive_int "show_size" req default

let offset (req:request) (default:int) : int =
  non_negative_int "offset" req default

let limit (req:request) (default:int) : int =
  non_negative_int "limit" req default

let jsonp (req:request) (default:int) : int =
  non_negative_int "jsonp" req default