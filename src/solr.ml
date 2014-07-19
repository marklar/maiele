open Http_client.Convenience
open Util;;  open Printf;;  open Pcre

(*
  Perform facet query of Solr.
  For one vertical, fetch:  (tag_id, glu_count) array
  Lsyncd uses this data to update its data structures.
*)

(* Query Solr for a particular vertical.  Response: (bogus) JSON *)
let fetch_in_stock_glu_counts (vertical_name:string) : string (*json*) =
  let uri =
    (sprintf "http://%s:%d/solr/select?" Cfg.solr_host Cfg.solr_port) ^
      String.concat "&"
      [ "facet.sort=index"               (* sort by index -- IMPORTANT *)
      ; "qt=tag_id_facets"               (* query type ("dismax"): ??? *)
      ; "fq=vertical:" ^ vertical_name   (* facet query: vertical *)
      ; "fq=is_scrollable:true"          (* facet query: is_scrollable *)
      ; "wt=json"                        (* response in (bogus) JSON format *)
      ]
  in http_get uri

(*
  Since Solr's response isn't proper JSON,
  we cannot easily use Json-Wheel to parse it.
  So instead, we use regexps to extract the data we want.
  extract() returns an Array of matches.  Take the first.
*)
let tag_id_list_rex = regexp "\\{\"tag_id_list\":\\[.*?\\]\\}"
let tag_id_list_str (json_str:string) : string =
  match extract json_str ~rex:tag_id_list_rex with
    | [||] -> ""
    | ary -> ary.(0)

(*
  id_count_list_str: flattened assoc list.  (An even number of \d+.)
  Convert into: (int * int) array.
*)
let tag_id_and_glu_count_rex = regexp "\"\\d+\",\\d+"
let int_rex = regexp "\\d+"
let pair_ary (id_count_list_str:string) : (int * int) array  =
  Array.map
    (function
       | [|s|] -> ( match extract_all s ~rex:int_rex with
		      | [| [|id_str|]; [|num_str|] |] ->
			  (int_of_string id_str, int_of_string num_str)
		      | _ -> failwith "what happened?" )
       | _ -> failwith "huh?"
    )
    (extract_all id_count_list_str ~rex:tag_id_and_glu_count_rex)

(*
  1. Query Solr.
  2. Extract string of comma-separated nums from JSON response.
  3. Convert into (int * int) array:  (TAG_ID, GLU_COUNT)
*)
let tag_2_num_glus_ary (vertical_name:string) : (int * int) array =
  let json = fetch_in_stock_glu_counts vertical_name in
  let part_of_json = tag_id_list_str json in
    pair_ary part_of_json
