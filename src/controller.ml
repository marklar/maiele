open Http_types;;  open Pcre;;  open Util;;  open Printf
open ExtList

let ok_domain_vals:string list = "all" :: Cfg.all_domain_names

let results (req:Http_types.request) (search:Search_fun.t)
    (format:Request.format) : string =
  let query_str    = Request.query_str    req
  and offset       = Request.offset       req 0
  and limit        = Request.limit        req 20
  and in_stock_p   = Request.bool_param   req "in_stock"
  and sellable_p   = Request.bool_param   req "sellable"
  and show_size    = Request.show_size    req 5
  and bop_p        = Request.bool_param   req "bop"
  and pc_p         = Request.bool_param   req "pc"
  and domain_names = Request.domain_names req Cfg.all_buy_domain_names
  in
  let opts =
    Query_opts.make show_size offset limit in_stock_p sellable_p bop_p pc_p format
  in search query_str domain_names opts

(* default domain: games *)
let domain_from_request (req:Http_types.request) : Domain.t =
  let dom_name = Request.domain_name req "games" in
    Searcher.domain_of_name dom_name
  
let text (req:Http_types.request) : string =
  let id = Request.int_param req "id"
  and dom = domain_from_request req in
    sprintf "\"%s\"" (Domain.text dom id)

let results_for_target (req:Http_types.request) : string =
  let target_id = Request.int_param req "target_id"
  and dom = domain_from_request req in
    Domain.results_str_for_target dom target_id

let range (req:Http_types.request) : string =
  let res_strs =
    let offset = Request.offset req 1
    and limit  = Request.limit  req 20
    and order  = Domain.Length
    and dom = domain_from_request req in
      List.map
	(fun r -> Result.range_json r (Domain.name dom))
	(Domain.range dom order offset limit)
  in sprintf "[%s]" (String.concat ",\n" res_strs)

let update_glu_in_stock (req:Http_types.request) : string =
  let glu_id = Request.int_param req "glu_id"
	and in_stock = Request.bool_param req "in_stock" in
		Tag_in_stock.set_in_stock glu_id in_stock;
		"success"

let charities_count (req:Http_types.request) (format:Request.format)
    : string =
  let pairs =
    let query_str = Request.query_str req in
    let count =
      let dom = Searcher.domain_of_name "charities"
      and query = Query.from_string false query_str
      in Charities_domain.count dom query
    in [ ("query", query_str)
       ; ("count", string_of_int count)
       ]
  in Show.show_pairs format (Some "charities") pairs

let hello (req:Http_types.request) : string =
  sprintf
    "<html>\
       <body>\
         <h1>Hello, %s!</h1>\
       </body>\
     </html>"
    (req#param ~default:"World" "name")


(*-- public --*)

type action_name = Hello
		 | Results
		 | CharitiesCount
		 | TargetIds
		 | Text
		 | ResultsForTarget
		 | Range
		 | UpdateGluInStock
		     (* problem cases *)
		 | Empty
		 | Unsupported

let path_re = regexp "/maiele/(.*?)(\\.html?|\\.xml|\\.js(on)?|\\?|$)"
let action_name (req:Http_types.request) : action_name =
  match extract req#path ~rex:path_re ~full_match:false with
    | [||] -> Empty
    | ary  -> match ary.(0) with
	| "hello"              -> Hello
	| "results"            -> Results
	| "charities_count"    -> CharitiesCount
	| "target_ids"         -> TargetIds
	| "text"               -> Text
	| "results_for_target" -> ResultsForTarget
	| "range"              -> Range
	| "update_glu_in_stock" -> UpdateGluInStock
	| _                    -> Unsupported

let response_body (format:Request.format) (req:Http_types.request)
    (search:Search_fun.t) : string =
  (*
    Logger.debug (match format with
      | `Json -> "json"
      | `Xml  -> "xml"
      | `Html -> "html") None;
  *)

  match action_name req with

    (* problem *)
    | Empty       -> raise $ Request.Invalid_path ""
    | Unsupported -> raise $ Request.Invalid_path (req#path)

    (* ok *)

    | Hello          -> hello req
    | Results        -> results req search format
    | CharitiesCount -> charities_count req format
	
    (* For creating:
     *   - "Listing.new lineup".
     *   - bestseller_importer (not yet in use).
     * id=1234 & domain=games
     *)
    | TargetIds -> Data_service.target_ids req
	
    (* id=1234 & domain=games *)
    | Text -> text req
	
    (* target_id=1234 & domain=games *)
    | ResultsForTarget -> results_for_target req
	
    (* values start at _1_ *)
    | Range -> range req

    (* glu_id=1234 & in_stock=true *)
    | UpdateGluInStock -> update_glu_in_stock req
