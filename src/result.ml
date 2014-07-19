open Printf;;  open Pcre;;  open Util;;  open ExtList

type t =
    { id            : int
    ; pop           : int
    ; is_faux       : bool
    ; text          : string   (* for prod verts, may contain contributor *)
    ; target_ids    : Int_ary.t
    ; display_product_code : string option  (* !! *)
    ; product_codes : string list
    ; in_stock_p    : bool option
    ; sellable_p    : bool option
    ; tags          : Tag.t list option
    ; store_ids     : int list option
    }

let sort_by_pop (ts:t list) : t list =
  let by_pop a b = compare a.pop b.pop in
    List.sort ts ~cmp:by_pop

(* just for result_test.ml *)
let id            t = t.id
let pop           t = t.pop
let text          t = t.text
let is_faux       t = t.is_faux
let target_ids    t = t.target_ids
let in_stock_p    t = t.in_stock_p
let sellable_p    t = t.sellable_p
let product_codes t = t.product_codes
let display_product_code t = t.display_product_code
let tags          t = t.tags
let store_ids     t = t.store_ids

let with_display_product_code t (s:string) : t =
  { t with display_product_code = Some s }

let with_in_stock t (p:bool) : t =
  { t with in_stock_p = Some p }
  
let with_sellable t (p:bool) : t =
  { t with sellable_p = Some p }

(*--- Creating ---*)

let create (id:int) (pop:int) (text:string)
    (target_ids:Int_ary.t) (is_faux:bool)
    (product_codes:string list)
    (tags:Tag.t list option) (store_ids:int list option) : t =
  { id            = id
  ; pop           = pop
  ; is_faux       = is_faux
  ; text          = text
  ; target_ids    = target_ids
  ; in_stock_p    = None  (* not provided *)
  ; sellable_p    = None  (* not provided *)
  ; display_product_code = None
  ; product_codes = product_codes
  ; tags          = tags
  ; store_ids     = store_ids
  }

(*--- JSON ---*)
let quote_re = regexp "\""
let quote_tm = subst  "\\\""
let esc_quotes s = replace ~rex:quote_re ~itempl:quote_tm s

(*
  For 'faux' results (games_platforms),
  includes an appropriate 'faux' contributor.
  (ToDo: put those strings in a CFG file?)
*)
let non_searchable_str t (dom_name:string) : string =
  let html = sprintf " <span class='non_searchable'>(%s)</span>" in
    if t.is_faux then
      match String.lowercase dom_name with
	| "games_platforms" -> html "any platform"
	| _ -> ""
    else
      ""
(*
  The 'text' field, possibly...
    - emboldened
    - with emboldened display_product_code
*)
let display_text t (regexps:Pcre.regexp list) (dom_name:string) : string =
  let bold = 
    let str = match t.display_product_code with
      | None -> t.text
      | Some s -> sprintf "%s : %s" s t.text
    in Embolden.embolden str regexps
  and rest = non_searchable_str t dom_name
  in sprintf "\"%s\"" (esc_quotes (bold ^ rest))

(*
  For "bop" (inline store), much less data is needed.
*)
let bop_show t (format:Request.format) (regexps:Pcre.regexp list)
    (dom_name:string) : string =
  let pairs = [ ("target_ids", Int_ary.to_json_ary t.target_ids)
	      ; ("text",       display_text t regexps dom_name)
	      ]
  in Show.show_pairs format (Some "result") pairs

(* these funs : (t -> (string * string) option) *)

let display_product_code_pair t = match t.display_product_code with
  | Some c -> Some ("code", sprintf "\"%s\"" c)
  | None -> None

let stock_pair t = match t.in_stock_p with
  | Some p -> Some ("in_stock", string_of_bool p)
  | None -> None  

let sellable_pair t = match t.sellable_p with
  | Some p -> Some ("sellable", string_of_bool p)
  | None -> None

let tags_pair (format:Request.format) t = match t.tags with 
  | Some tags ->
      let tag_strs = List.map (fun tag -> Tag.show tag format) tags
      in Some ("tags", Show.show_list format tag_strs)
  | None -> None

let avail_glus_pair t = match t.tags with
  | Some tags -> Some ("avail_glus", string_of_int $ Tag.sum_avail_glus tags)
  | None -> None

let total_glus_pair t = match t.tags with
  | Some tags -> Some ("total_glus", string_of_int $ Tag.sum_total_glus tags)
  | None -> None

let charity_id_pair (dom_name:string) t =
  match dom_name with
    | "charities" -> Some ("charity_id", string_of_int $ Int_ary.hd t.target_ids)
    | _ -> None

(*
  For regular search.  Show all data for a result.
  Results OPTIONally have:
    - in_stock    : for verticals, only if requested
    - sellable    : for verticals, only if requested
    - tags        : browse only
    - num_glus    : browse only
    - charity_ids : charities only
*)
let non_bop_show t (format:Request.format) (regexps:Pcre.regexp list)
    (dom_name:string) : string = 
  let opts = List.filter_map (fun f -> f t)
    [ tags_pair format
    ; stock_pair
    ; sellable_pair
    ; display_product_code_pair
    ; avail_glus_pair
    ; total_glus_pair
    ; charity_id_pair dom_name
    ]
  and pairs = [ ("res_id", string_of_int t.id)
	      ; ("pop",    string_of_int t.pop)
	      ; ("text",   display_text t regexps dom_name)
	      ]
  in Show.show_pairs format (Some "result") (opts @ pairs)

(* public *)

let cmp_pop (a:t) (b:t) : int = compare a.pop b.pop

(*
  Determine whether to show a particular 'browse' result,
  based on how many Glus are in stock for that result.
  Configurable (:browse_min_glus).
*)
let too_few_glus_p t : bool =
  match t.tags with
    | None -> false
    | Some tags -> Tag.sum_avail_glus tags < Cfg.browse_min_glus  (*browse*)

let show t (regexps:Pcre.regexp list) (dom_name:string) (opts:Query_opts.t)
    : string =
  let format = Query_opts.format opts
  and f =
    if Query_opts.bop_p opts then
      bop_show
    else
      non_bop_show
  in f t format regexps dom_name

(* convert these to do xml, too *)

let to_unadorned_json t (dom_name:string) : string =
  non_bop_show t `Json [] dom_name

let range_json t (dom_name:string) : string =
  let pairs = [ ("res_id",     string_of_int t.id)
	      ; ("pop",        string_of_int t.pop)
	      ; ("is_faux",    string_of_bool t.is_faux)
	      ; ("target_ids", Int_ary.to_json_ary t.target_ids)
	      ; ("text",       display_text t [] dom_name)
	      ]
  in Show.show_pairs `Json (Some "result") pairs

let add_in_stock_info (is_in_stock:t -> bool) (t:t) : t =
  with_in_stock t (is_in_stock t)

let add_sellable_info (is_sellable:t -> bool) (t:t) : t =
  with_sellable t (is_sellable t)
