open Printf;;  open ExtList;;  open Pcre
open Util;;  open Logger

type query = string
type domain_names = string list
type json = string

type t = query -> domain_names -> Query_opts.t -> json

(** (domain name, results) *)
type domain_results_pair = (string * Result.t list)

(*
  In-stock info is relevant for only certain domains.
  For those domains, gather results.
  NB: These are the same domains that have relevance for
  sellability, so the same filtering function is used.
*)
let res_for_all_in_stock_domains (dom_res_pairs:domain_results_pair list)
    : Result.t list =
  List.concat $
    (List.map snd $
       List.filter (Cfg.is_in_stock_domain |> fst) dom_res_pairs)

(*
  Create fun:  Given a Result, is it in stock?

  Find in-stock Glu IDs.
    - gather up Glu IDs
    - query DB: which are in stock?  MAY RAISE.
  Make set of Results that correspond to those Glus.

  Enclose over that set of Results.
  For any given Result, check for membership.
*)
let in_stock_exn_fun (dom_res_pairs:domain_results_pair list) : Result.t -> bool =
  let res = res_for_all_in_stock_domains dom_res_pairs in
    In_stock.has_in_stock_glu_fun res  (* may raise *)

(*
  Create fun:  Given a Result, is it sellable?

  Find sellable Glu IDs.
    - gather up Glu IDs
    - query DB: which are sellable?  MAY RAISE.
  Make set of Results that correspond to those Glus.

  Enclose over that set of Results.
  For any given Result, check for membership.
*)
let sellable_exn_fun (dom_res_pairs:domain_results_pair list) : Result.t -> bool =
  let res = res_for_all_in_stock_domains dom_res_pairs in
    Is_sellable.has_sellable_glu_fun res  (* may raise *)

(*
  For some domains, we may want to add in-stock info to its results.
  do so based on function described above.
*)
let add_in_stock_info (dom_res_pairs:domain_results_pair list)
    : domain_results_pair list =
  try
    let is_in_stock = in_stock_exn_fun dom_res_pairs in
      List.map
	(fun (n,rs) -> let rs' = match n with
	   | "browse" ->      (* already filtered *)
	       rs
	   | "charities" ->   (* ditto, but decorate anyway.  WHY? *)
	       List.map (Result.add_in_stock_info (fun _ -> true)) rs
	   | _ ->             (* _actually_ add in_stock info *)
	       List.map (Result.add_in_stock_info is_in_stock) rs
	 in (n,rs'))
	dom_res_pairs
  with _ ->
    warn "Failed to add in_stock info to results." None;
    dom_res_pairs

(*
  For some domains, we may want to add sellable info to its results.
  do so based on function described above.
*)
let add_sellable_info (dom_res_pairs:domain_results_pair list)
    : domain_results_pair list =
  try
    let is_sellable = sellable_exn_fun dom_res_pairs in
      List.map
	(fun (n,rs) -> let rs' = match n with
	   | "browse" | "charities" ->      (* already filtered *)
	       rs
	   | _ ->             (* _actually_ add in_stock info *)
	       List.map (Result.add_sellable_info is_sellable) rs
	 in (n,rs'))
	dom_res_pairs
  with _ ->
    warn "Failed to add sellable info to results." None;
    dom_res_pairs

(*
  Select Result.t fields, based on domain and bop_p.
  Embolden text (if requested).
*)

let show (domain_results_pairs:domain_results_pair list) (query:string)
    (opts:Query_opts.t) : json =
  let format = Query_opts.format opts
  and strs =
    (* let re = Embolden.make_regexp query in *)
    let regexps = Embolden.make_regexps query in
    let show' (name, results) = Searcher.show name results regexps opts
    in List.map show' domain_results_pairs
  in Show.show_list format strs

(*
  For each domain_name, fetch results.
  Return an assoc list (name, results).
*)
let get_domain_results_pairs (query_str:string) (domain_names:string list)
    (opts:Query_opts.t) : domain_results_pair list =
  let query = Query.from_string (Query_opts.pc_p opts) query_str
  in
    List.map 
      (fun dn ->
	 let res =
	   duration dn (fun () -> Searcher.results query dn opts)
	 in
	   if res = [] then info (sprintf "%s []: %s" dn query_str) None;
	   (dn, res)
      )
      domain_names

(*
  Log failed searches (i.e. no results for any domain).
  To see if we can improve searching experience.

  (If only we knew what domain they meant to be searching over;
  then we could look for domain-specific failed searches.)
*)
let log_no_results (query:string) (dom_res_pairs:domain_results_pair list) : unit =
  let no_results = function
    | (_, []) -> true
    | _ -> false
  in
    if List.for_all no_results dom_res_pairs then
      info ("No results: " ^ query) None

(*-- public --*)

(*
  'in_stock_p' has 2 different meanings, depending on the search domain.
     - charities: FILTER results by whether the charity has any inventory.
     - verticals: decorate results w/ whether in_stock or not.
*)

(***
let make () : t =
  fun query_str domain_names opts ->
    let dom_res_pairs:domain_results_pair list =
      let drs =  (* get results *)
	get_domain_results_pairs query_str domain_names opts
      in
	(* add in-stock data? *)
	if Query_opts.in_stock_p opts then
	  add_in_stock_info drs
	else
	  drs
    in
      (* return string *)
      log_no_results query_str dom_res_pairs; 
      show dom_res_pairs query_str opts
***)

let decorate_domain_results_pairs (dom_res_pairs:domain_results_pair list) (opts:Query_opts.t): domain_results_pair list =
  let drs = 
	  (* add in-stock data? *)
	  if Query_opts.in_stock_p opts then
	    add_in_stock_info dom_res_pairs
	  else
	    dom_res_pairs
  in
    if Query_opts.sellable_p opts then
      add_sellable_info drs
    else
      drs

(* type t *)
let search (query_str:string) (domain_names:string list) (opts:Query_opts.t) : string =
  let dom_res_pairs:domain_results_pair list =
    let drs =  (* get results *)
      get_domain_results_pairs query_str domain_names opts
    in
      decorate_domain_results_pairs drs opts
  in
    (* return string *)
    log_no_results query_str dom_res_pairs; 
    show dom_res_pairs query_str opts
