open Util;; open Printf;; open Logger

(*
For updating the "browse" index's Tag_tbl, to keep in sync w / database.

Each tag has a corresponding count of available Glus.
We use that 'avail_glus' number to determine whether to display
a given tag in our "browse" results.
*)

let dir_name = Cfg.idx_root_dir ^ "/browse"
let file_name = "tag.avail_glus.data"
let vertical_names: string list = Cfg.all_sell_domain_names

(*
1. Fetch (tag_id, glu_count) pairs from Solr.
2. Update 'ary' in place, using that data.
*)
let update_ary_for_vertical (ary: int array) (v: string) : unit =
	let tag_2_num_glus_ary =
		try
			let (s, res) = time_and_res (fun () -> Solr.tag_2_num_glus_ary v) in
				log "Fetching" s;
				res
		with _ -> warn (sprintf "Failure to fetch info for %s." v) None; [||]
	in Array.iter
			(fun (tag_id, num_glus) ->
						try ary.(tag_id - 1) <- num_glus
						with _ -> warn (sprintf "Solr tag #%d: out of idx file's range." tag_id) None
			)
			tag_2_num_glus_ary

(*
Given number of tags ('len'), create a single, zero - ed out Array.
For each vertical, update values for that vertical's tags.
*)
let create_one_ary_for_all_verticals (len: int) : int array =
	let ary = Array.make len 0 in
		List.iter
			(fun n ->
						let (s, _) = time_and_res (fun () -> update_ary_for_vertical ary n) in
							log (sprintf "Updating %s" n) s)
			vertical_names;
		info "Finished creating ary." None;
		ary

(*
We do not use Int_ary.of_array because...
We already have a particular (updatable, file - backed) Int_ary.t.
We want to copy values into it, rather than create a new one.
NB: updates 'ia' in situ.
*)
let blit (ary: int array) (ia: Int_ary.t) : unit =
	Array.iteri (Int_ary.safe_set ia) ary;
	info "Finished updating Int_ary." None

(*
If Some x, then we're in the middle of an update,
and if we catch any signals, we'll need to close the Int_ary.
If None, then nothing to close.
*)
let global_ia_ref = ref None

(*
1. Open an updatable, file - backed Int_ary.t. Get its length.
2. Create an int array of that length.
Fill it with values from Solr searches (1 per vert).
3. "Blit" the int array's contents into the Int_ary.t.
4. Close the Int_ary.ml (to ensure changes be flushed).
*)
let do_all () : unit =
	let ia = Int_ary.from_file ~shared: true dir_name file_name in
		global_ia_ref := Some ia;   (* in case we catch a signal *)
		let ary = create_one_ary_for_all_verticals $ Int_ary.length ia in
			blit ary ia;
			Int_ary.close ia;
			global_ia_ref := None

let _ =
	Logger.create_log_file "lsync";
	
	(* Set up signal handlers, so we can exit safely
	(i.e. without corrupting the data file).
	*)
	Sys.set_signal Sys.sigint
		(Sys.Signal_handle
			(fun _signum -> info "Caught SIGINT." None; exit 0) );
	Sys.set_signal Sys.sigterm
		(Sys.Signal_handle
			(fun _signum -> info "Caught SIGTERM." None; exit 1) );
	at_exit
		(fun () ->
					( match !global_ia_ref with
						| Some ia ->
								info "Closing file." None;
								Int_ary.close ia
						| None ->
								info "File already closed." None);
					info "Exiting." None
		);

	info "--- STARTING lsync ---" None;
  do_all ()
