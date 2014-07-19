open Util;; open Printf;; open ExtList

(*
Searcher provides searching API, to Search_fun and Controller.
Knows about all Domains.
*)

(*
Config file (cfg /.config.js) may contain info for a domain
whose index doesn't actually exist.
In such cases, simply skip it.
*)
let domains : Domain.t list =
	let create_domain (idx: Cfg.index) : Domain.t option =
		try
			let dir = Cfg.idx_root_dir ^ (Cfg.idx_dir idx) in
				Some (Domain.create (Cfg.idx_name idx) dir)
		with _ ->
				None
	in List.filter_map create_domain Cfg.indices

(*-- public --*)

let close () : unit =
	List.iter Domain.close domains

(* May raise Not_found. *)
let domain_of_name (domain_name: string) : Domain.t =
	let has_right_name =
		let dn = String.lowercase domain_name in
			(fun d -> String.lowercase (Domain.name d) = dn)
	in List.find has_right_name domains

let show (domain_name: string) (rs: Result.t list) (regexps: Pcre.regexp list)
		(opts: Query_opts.t) : string =
	let format = Query_opts.format opts in
		let pairs =
			let result_strs =
				List.map (fun r -> Result.show r regexps domain_name opts) rs
			in [ ("type", sprintf "\"%s\"" domain_name)
				; ("results", Show.show_list format result_strs)
				]
		in Show.show_pairs format (Some "result") pairs

(* in_stock_p: for FILTERING charities results *)
let results (query: Query.t) (domain_name: string) (opts: Query_opts.t)
: Result.t list =
	try
	(* assert (domain_name <> "all"); *)
		let d = domain_of_name domain_name in
			if Query_opts.in_stock_p opts && domain_name = "charities" then
				Charities_domain.in_stock_results d query opts
			else
				Domain_searcher.results d query opts
	with exc ->
	(* log exception *)
    Logger.warn (sprintf "Error while searching: %s" (Printexc.to_string exc)) None;
    []

(*
SPECIFICALLY FOR A GIVEN DOMAIN, RES_ID.
For creating LINE - UP.
*)
let target_ids (domain_name: string) (result_id: int) : Int_ary.t =
	Domain.target_ids (domain_of_name domain_name) result_id

(* For Data_service: Bestseller Import. *)
let target_ids_for_query (query_str: string) (domain_name: string)
		(opts: Query_opts.t) : Int_ary.t list =
	assert (domain_name <> "all");
	let domain = domain_of_name domain_name
	and query = Query.from_string (Query_opts.pc_p opts) query_str in
		let rs = Domain_searcher.results domain query opts in
			List.map Result.target_ids rs
