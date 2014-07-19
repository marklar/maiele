open ExtList
open Util
open Printf

type t = Domain.t

(*
One 'type' [id or pop].
One query tree [avec or sans].
*)
let get_results (num: int) (fetcher: Result_fetcher.t) (filter: Filter_fun.t)
: Result.t list =
	let rec loop (acc: Result.t list) (num': int) =
		if num' <= 0 then
			acc
		else
			match fetcher () with
			| None -> acc
			| Some result -> match filter result with
					| None -> loop acc num'
					| Some r -> loop (r:: acc) (num'- 1)
	in
		List.rev (loop [] num)

type order = Length | Pop

(*
Gets as many 'withs' as it can.
Backfills with 'withouts'.
*)
let get_withs_then_withouts t (order: order) (num: int)
		(avec_qt: Id_query_tree.t) (sans_qt_opt: Id_query_tree.t option)
		(filter: Filter_fun.t) : Result.t list =
	
	(*
	The only difference between the two searches ("withs" and "withouts") is:
	+ n: how many Result.ts to grab, and
	+ qt: which Id_query_tree.t to use
	*)
	let get (n: int) (qt: Id_query_tree.t) : Result.t list =
		let fetcher = match order with
			| Length -> Result_fetcher.id_order_fun t qt
			| Pop -> Result_fetcher.pop_order_fun t qt
		in
			get_results n fetcher filter
	in
	
	(* Get "withs" Results.  Backfill with "withouts". *)
		let withs = get num avec_qt in
			let withouts = match sans_qt_opt with
				| None -> []
				| Some qt -> get (num - List.length withs) qt
			in
				withs @ withouts

(* documents *)
let get_shorts_and_pops t (str_query_tree: Str_query_tree.t) (filter: Filter_fun.t) (num: int) (opts: Query_opts.t) : Result.t list * Result.t list =
	let avec_qt = Id_query_tree.from_str_tree str_query_tree (Domain.lexicon t)
	and sans_qt_opt = Sans_id_query_tree.from_str_query_tree str_query_tree t
	in
		let shorts =
			get_withs_then_withouts t Length 1 avec_qt sans_qt_opt filter in
			let pops =
				let num_pop = num - (List.length shorts) in
					get_withs_then_withouts t Pop num_pop avec_qt sans_qt_opt filter
			in (shorts, pops)

let get_doc_results t (str_query_tree: Str_query_tree.t) (filter: Filter_fun.t)
		(num: int) (opts: Query_opts.t) : Result.t list =
	let (shorts, pops) =
		get_shorts_and_pops t str_query_tree filter num opts
	in
	(*
	Stick shortest lex matches into pops,
	- at best: where they belongs according to popularity, - OR -
	- at worst: at position 'show_size'.
	*)
		insert_multi_into_top_n
			(Query_opts.show_size opts) shorts pops Result.cmp_pop

let results_for_query t (query: Query.t) (filter: Filter_fun.t)
		(num: int) (opts: Query_opts.t)
: Result.t list =
	let doc_results = get_doc_results t (Query.doc_only query) filter num opts in
		let num_doc_results = List.length doc_results in
			match (num - num_doc_results) with
			| 0 -> doc_results
			| num' -> match (Domain.product_code_tbl t, Query.code_only_opt query) with
					| (None, _) | (_, None) -> doc_results
					| (Some _, Some code) ->
							let code_results = Product_code_search.results_for_domain t code num' in
								doc_results @ code_results

(*
Generally...

Select:
- X most popular
- 1 shortest
Sort:
- By pop. Except...
- Except, if shortest not among top X, put at position X.

However...
The user may make a query containing a token which matches the domain name.
e.g. "cds" in "madonna cds"

When that happens, do the query both WITH (avec) then possibly withOUT (sans) "cds",
and integrate results into a single list.
*)
let results t (query: Query.t) (opts: Query_opts.t) : Result.t list =
	let filter = Filter_fun.create opts in
		(* in case of non-zero offset *)
			List.drop
				(Query_opts.offset opts) (results_for_query t query filter (Query_opts.limit opts) opts)
