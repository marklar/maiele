(* Copyright (c) 2009, Mark Wong-VanHaren, Glyde Corp. *)

open Printf
open Http_types
open Pcre
open Request

let camel_char_re = regexp "([a-z0-9]+)([A-Z])"
let under_sub = subst "$1_$2"
let snake_case (str:string) : string =
  String.lowercase (replace ~rex:camel_char_re ~itempl:under_sub str)

(*
  the string is expected to be of this format: "1,2,3,4",
  with optional '[' and ']' around it.
*)
let comma_re   = regexp ","
let bracket_re = regexp "(\\[|\\])"
let empty_sub  = subst ""
let int_ary_of_string (str:string) : Int_ary.t =
  (* this is insane.  more effic way to do this? *)
  let strs =
    split ~rex:comma_re (replace ~rex:bracket_re ~itempl:empty_sub str)
  in
  let int32_list = List.map Int32.of_string strs in
    Int_ary.of_array (Array.of_list int32_list)


(*---- PUBLIC ----*)

(*********
(* Use HTTP POST, because passing large amounts of data (filter res_ids). *)
let result_ids (req:Http_types.request) : string (*json*) =
  let param s = req#param ~default:"" s in
  let query_str = param "query" in  (* in UTF-8.  default query? *)
  let generic_query_tree = Query_tree.of_string (to_latin1 query_str) in
    (*
  and domain_name_w_result_ids =  (* pair list *)
    List.map
      (fun dn ->
	 let key = (snake_case dn) ^ "_results" in
	 let ia = int_ary_of_string (param key) in
	   (dn, ia)
	     (*
	   log "just BEFORE marshal" 0.0;
	   let start_time = Unix.gettimeofday () in
	   let (ary:int32 array) = Marshal.from_string (param key) 0 in
	     log "just AFTER marshal" 0.0;
	     let ia = Int_ary.of_array ary in
	     let end_time = Unix.gettimeofday () in
	       log (sprintf "%s input size: %d" key (Int_ary.length ia)) (end_time -. start_time);
	       (dn, ia)
	     *)
      )
      Cfg.all_domain_names
  in
    *)
  let per_domain_jsons =
    List.map
      (* (fun (dn, ids) ->*)
      (fun dn ->
	 (* let qt = Query_tree.filter_by_res_ids generic_query_tree ids in
	    let res_ids = Searcher.all_result_ids qt dn in *)
	 let res_ids = Searcher.all_result_ids generic_query_tree dn in
	   (* log (sprintf "res_ids size: %d" (Int_ary.length res_ids)) 0.0; *)
	   sprintf "\"%s\": %s" dn (Int_ary.to_json_ary res_ids)
      )
      (* domain_name_w_result_ids *)
      Cfg.all_domain_names
  in
    sprintf
      "{\"success\":true, \"result_ids\":{%s}}"
      (String.concat ", " per_domain_jsons)
*********)

(* For creating LS data structures, the OLD way.
 * This whole module is soon to be deprecated.
 *)
let target_id_2_result_ids_json (req:Http_types.request) : string (*json*) =
  try
    (* we put "bobo" def values so as not to handle exceptions. *)
    let p s = req#param ~default:"bobo" s in
    let domain_name    = p "domain"
    and target_ids_str = p "target_ids"
    in
      (* convert target_ids_str into Int_ary.t. *)
    let target_ids = int_ary_of_string target_ids_str in
      if not (List.mem domain_name ("all" :: Cfg.all_domain_names)) then
	raise (Invalid_param ("domain", domain_name))
      else
	Searcher.target_id_2_result_ids_json domain_name target_ids
  with Invalid_param (name, value) ->
    sprintf
      "{\"success\":false, \"exception\":\"Invalid argument value.  Arg: '%s'.  Val: '%s'.\"}"
      name value
