open Http_types;;  open Pcre;;  open Util;;  open Printf

type json = string

let ok_domain_vals:json list = "all" :: Cfg.all_domain_names

let results (req:Http_types.request) (search:Jo_master.search_fun) : json =
  let query_str   = Request.query_str  req
  and offset      = Request.offset     req 0
  and limit       = Request.limit      req 20
  and in_stock_p  = Request.bool_param req "in_stock"
  and show_size   = Request.show_size  req 5
  and bop_p       = Request.bool_param req "bop"
  and domain_names = match Request.domain_names req ["all"] with
    | ["all"] -> Cfg.all_buy_domain_names
    | dns     -> dns
  in search query_str domain_names show_size offset limit in_stock_p bop_p

(* default domain: games *)
let domain_from_request (req:Http_types.request) : Domain.t =
  let dom_name = Request.domain_name req "games" in
    Searcher.domain_of_name dom_name
  
let text (req:Http_types.request) : json =
  let id = Request.int_param req "id"
  and dom = domain_from_request req in
    sprintf "\"%s\"" (Domain.text dom id)

let results_for_target (req:Http_types.request) : json =
  let target_id = Request.int_param req "target_id"
  and dom = domain_from_request req in
    Domain.results_for_target dom target_id

let range (req:Http_types.request) : json =
  let res_strs =
    let offset = Request.offset req 1
    and limit  = Request.limit  req 20
    and order  = Request.order  req "length"
    and dom = domain_from_request req in
      List.map
	(fun r -> Result.range_json r (Domain.name dom))
	(Domain.range dom order offset limit)
  in "[" ^ (String.concat ",\n" res_strs) ^ "]"

let charities_count (req:Http_types.request) : json =
  let count =
    let dom = Searcher.domain_of_name "charities"
    and query_tree = Str_query_tree.of_string (Request.query_str req)
    in Domain.charities_count dom query_tree
  in sprintf "{\"count\":%d}" count

(*-- public --*)

let maiele_re = regexp "/maiele/"
let empty_sub = subst ""
let trim_path_dirs = replace ~rex:maiele_re ~itempl:empty_sub

let response_body (req:Http_types.request) (search:Jo_master.search_fun) : json =
  match trim_path_dirs req#path with
    | "results.js"
    | "results.json" -> results req search

    | "charities_count.js"
    | "charities_count.json" -> charities_count req
	  
    (* For creating:
     *   - "Listing.new lineup".
     *   - bestseller_importer (not yet in use).
     * id=1234 & domain=games
     *)
    | "target_ids.js"
    | "target_ids.json" -> Data_service.target_ids req

    (* id=1234 & domain=games *)
    | "text.js"
    | "text.json" -> text req

    (* target_id=1234 & domain=games *)
    | "results_for_target.js"
    | "results_for_target.json" -> results_for_target req

    (* values start at _1_ *)
    | "range.js"
    | "range.json" -> range req

    (* error *)
    | x -> raise (Request.Invalid_path x)
