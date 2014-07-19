open Util

type t = Domain.t

let all_matching_results t (query:Query.t)
    : Result.t list =

  let opts =
    let limit = Result_tbl.length (Domain.result_tbl t) in
      Query_opts.make 5 0 limit false false false false `Json
  in Domain_searcher.results t query opts

(*
  Fetch ALL matching results.  Then count them.
  Not ideally efficient. :-)
*)
let count t (query:Query.t) : int =
  List.length $ all_matching_results t query

(*
  Specifically for charities.
  1. Fetch ALL matching results.
  2. Filter by whether charity's storefront has any glus in-stock.
  3. Drop offset; take limit.
*)
let in_stock_results t (query:Query.t)
    (opts:Query_opts.t) : Result.t list =
  let (off,lim) = (Query_opts.offset opts, Query_opts.limit opts) in
    if lim <= 0 then
      []
    else
      let all_rs = all_matching_results t query in
      let has_inventory = Store_in_stock.has_inventory_fun all_rs in
	sub (List.filter has_inventory all_rs) off lim
