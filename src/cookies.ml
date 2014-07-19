
let cookie_re = Pcre.regexp "[ \t]*;[ \t]*"
let name_val_re = Pcre.regexp "^([a-zA-Z0-9_.]+)(=(.*))?$"

type t = (string * string)

(*
  From string like this:
     - <name>  OR
     - <name>=<value>
  return (name, value).

  If value is absent: "".
*)
let extract_name_and_val (str:string) : t =
  match Netstring_pcre.string_match name_val_re str 0 with
    | None ->
	failwith ("get_set_cookie: " ^ str)
    | Some m ->
	let name =
	  Netstring_pcre.matched_group m 1 str
	and value = 
	  try Netstring_pcre.matched_group m 3 str
	  with Not_found -> ""
	in
	  (name, value)

let downcase_names (params:t list) =
  let down (n,v) = (String.lowercase n, v) in
    List.map down params

let name_val_pairs (str:string) : t list =
  List.map
    extract_name_and_val
    (Pcre.split ~rex:cookie_re str)   (* split at ';' *)


(*--- UNUSED ---*)

let get_set_cookie (str:string) =
  match name_val_pairs str with
    | (n,v) :: params ->
	let params' = downcase_names params in

	  { Nethttp.cookie_name = Netencoding.Url.decode ~plus:false n

	  ; cookie_value = Netencoding.Url.decode ~plus:false v

	  ; cookie_expires = (
	    try
	      let exp_str = List.assoc "expires" params' in
		Some (Netdate.since_epoch (Netdate.parse exp_str))
	    with Not_found ->
	      None
	  )

	  ; cookie_domain = (
	    try
	      Some(List.assoc "domain" params')
	    with Not_found ->
	      None
	  )

	  ; cookie_path = (
	    try
	      Some(List.assoc "path" params')
	    with Not_found ->
	      None
	  )

	  ; cookie_secure = (
	    try
	      List.mem_assoc "secure" params'
	    with Not_found ->
	      false
	  )

	  }
    | _ ->
	failwith "get_set_cookie"


let get_set_cookies mh =
  List.map get_set_cookie (mh#multiple_field "set-cookie")

let set_cookies mh l = 
  Nethttp.Header.set_cookie mh 
    (List.map (fun c -> c.Nethttp.cookie_name, c.Nethttp.cookie_value) l)
