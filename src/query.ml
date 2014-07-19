open Util
open ExtList
open Printf

type t =
	{ doc_only : Str_query_tree.t
		; code_only_opt : string option
	}

let doc_only t = t.doc_only
let code_only_opt t = t.code_only_opt

let hyphen_re = Pcre.regexp "-"
let empty_sub = Pcre.subst ""
let rm_hyphens (s: string) : string =
	Pcre.replace s ~rex: hyphen_re ~itempl: empty_sub

(* debug, OUnit *)
let show t : string =
	let code_str = match t.code_only_opt with
		| None -> "None"
		| Some s -> s
	and doc_str = Str_query_tree.show t.doc_only
	in
		sprintf "code: %s.  doc: %s." code_str doc_str

let code_re = Pcre.regexp "^\\s*([\\d|-]+)\\s*$"
let code_from_str (query_str: string) : string option =
	try
		match Pcre.extract query_str ~rex: code_re ~full_match: false with
		| [| code |] ->
			match rm_hyphens code with
				| "" -> None
				| s -> Some s
		| _ -> failwith "Huh?"
	with Not_found ->
			None

let from_string (product_code_p: bool) (query_str: string) : t =
	{ doc_only = Str_query_tree.of_string query_str
  ; code_only_opt =
	  if product_code_p then
	    code_from_str query_str
	  else
	    None
	}