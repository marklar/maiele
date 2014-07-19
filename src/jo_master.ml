open Printf;;  open Util;;  open ExtList;;  open Pcre

(*
 * All functions in this module are called only once.
 * The goal is to create a single function (string -> string)
 * for executing a serial/parallel search.
 *)

type query = string
type domain_names = string list
type json = string

type search_fun = query -> domain_names -> int -> int -> int -> bool -> bool -> json

exception Timeout

(** domain name, results *)
type name_w_results = (string * Result.t list)

type t = { job_done : (Str_query_tree.t * float * name_w_results) Join.chan
	 ; need     : int Join.chan
	 ; reset    : (unit -> unit)
	 ; wait     : (unit -> name_w_results list)
	 }

(* Create master's Join.chans.  JoCaml "Collector" pattern.
 *  asynchronous
 *     - receive:  is sent to minions, so they know where to send back result.
 *     - need:     prep expectations, which may change each time.
 *  synchronous
 *     - wait:     synchronously await results (result list list) from minions.
 *  hidden
 *     - gathered: accumulates results.
 *)
let create_master (log:Logger.t) : t =
  def receive(query, secs, (dom_name, rs)) & need(n) & gathered(acc_time, acc_results) =
    log (sprintf "%s.  %s" (Str_query_tree.show query) dom_name) secs;
    gathered(acc_time +. secs, (dom_name, rs)::acc_results) & need(n-1)
  or wait() & need(0) & gathered(acc_time, acc_results) =  (*reaction rule*)
    log "total" acc_time;
    gathered(0., []) & reply acc_results to wait
 in
   let reset () = spawn gathered(0., []) in
     reset();
     { job_done = receive
     ; need     = need
     ; reset    = reset
     ; wait     = wait
     }
       
let update_in_stock (name_w_results_pairs:name_w_results list) (log:Logger.t) : name_w_results list option =
  try
    let vert_is_sellable =   (* create single fun for all verts; thus hit DB only once. *)
      let all_vertical_res =
	List.concat $ (List.map snd $ List.filter (Cfg.is_vertical |> fst) name_w_results_pairs)
      in In_stock.has_sellable_glu_fun all_vertical_res log
    in 
      Some (List.map
	      ( fun (n,rs) -> let rs' = match n with
		  | "browse"    -> rs      (* already filtered *)
		  | "charities" -> List.map (Result.update_in_stock (fun _ -> true)) rs  (* ditto, but decorate anyway *)
		  | _           -> List.map (Result.update_in_stock vert_is_sellable) rs
		in (n,rs') )
	      name_w_results_pairs)
  with _ -> None

let reduce (query:string) (in_stock_p:bool) (bop_p:bool) (name_w_results_pairs:name_w_results list) (log:Logger.t)
    : json =
  let jsons =
    let rex = Embolden.make_regexp query
    and (show_in_stock_p, updated_name_w_results_pairs) =
      if in_stock_p then
	match update_in_stock name_w_results_pairs log with
	  | Some rs -> (true, rs)
	  | None ->
	      log "WARNING: Failed to update results with in_stock info." 0.;
	      (false, name_w_results_pairs)
      else (false, name_w_results_pairs) in
    let jsonize (name, results) = Searcher.jsonize name results show_in_stock_p bop_p rex in
      List.map jsonize updated_name_w_results_pairs
  in sprintf "[%s\n]" (String.concat ",\n" jsons)

(*-- public --*)

(* Create a function to perform parallel searches. *)
let parallel_search_fun (join_port:int) (reg_chan_name:string) (log:Logger.t)
    : search_fun =
  let minions =
    let num_domains = List.length Searcher.domains in
      Jo_minion.channel() :: Jo_minion.create_n (num_domains-1) join_port reg_chan_name
  and master = create_master log in
    fun query dom_names show_size offset limit in_stock_p bop_p ->
      let (name_w_results_pairs:name_w_results list) = 
	let query_tree = Str_query_tree.of_string query 
	and bop_limit = limit + if bop_p then 1 else 0
	in match dom_names with  (* just 1 domain -> don't need minions. *)
	  | dn::[] -> [(dn, Searcher.results ~domain_name:dn ~show_size ~offset ~limit:bop_limit ~in_stock_p query_tree)]
	  | _ -> 
	      let scatter () =
		let num_ds = List.length dom_names in
		  spawn master.need(num_ds);
		  List.iter2
		    (fun minion dn -> spawn minion(query_tree, dn, show_size, offset, bop_limit, in_stock_p, master.job_done) )
		    (List.take num_ds minions)
		    dom_names
	      and gather () =
		match Jo_util.timeout Cfg.jo_timeout_secs master.wait with
		  | Some r -> r
		  | None -> 
		      log (sprintf "TIMEOUT: \"%s\"" query) Cfg.jo_timeout_secs;
		      exit 3  (* why 3?  just cuz. *)
	      in
	      let (secs, res) = time_and_res (fun () -> scatter() ; gather()) in
		log "scatter + gather" secs;
		res
      in reduce query in_stock_p bop_p name_w_results_pairs log

let serial_search_fun (log:Logger.t) : search_fun =
  fun query dom_names show_size offset limit in_stock_p bop_p ->
    let (name_w_results_pairs:name_w_results list) = 
      let query_tree = Str_query_tree.of_string query
      and bop_limit = limit + if bop_p then 1 else 0
      in List.map 
	   (fun dn ->
	      let (secs, pair) = time_and_res
		(fun () -> (dn, Searcher.results ~domain_name:dn ~show_size ~offset ~limit:bop_limit ~in_stock_p query_tree) )
	      in
		log (fst pair) secs;
		pair)
	   dom_names
    in reduce query in_stock_p bop_p name_w_results_pairs log
