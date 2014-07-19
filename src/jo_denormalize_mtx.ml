(*
 * For each lexeme, merge into its ids and pops
 * those values from its shortest superstrings.
 *)

open Util;;  open Printf;;  open ExtList

(* cfg: begin *)
let (dir_name, tbl_num) =
  try
    (Sys.argv.(1), int_of_string (Sys.argv.(2)))
  with _ ->
    failwith (sprintf "usage: %s <dir_name> <tbl_num> <num_minions>" Sys.argv.(0))
(* cfg: end *)

type posting_ary = Int_ary.t

let empty_posting_ary:posting_ary = Int_ary.empty

let lexicon = Lexicon.open_tbl dir_name "lex"
let num_lxms = (Lexicon.length lexicon) + 1

let out_tbl_name kind =
  sprintf "mtx.%d.%s" tbl_num kind

let in_tbl_name kind ch_code =
  (out_tbl_name kind) ^ (sprintf ".%d" ch_code)

let data_file_name (kind:string) (ch_code:int) =
  (in_tbl_name kind ch_code) ^ ".data"

(* input *)
let tbl = Int_ary_tbl.open_tbl dir_name (out_tbl_name "ids")

(* Does NOT include lxm_id. *)
let super_posting_arys (lxm_id:int) (posting_arys:posting_ary array) (first_lxm_id:int) : posting_ary list =
  let super_ids =
    let lxm = Lexicon.get_exn lexicon lxm_id in
      Lexicon.shortest_super_ids lexicon lxm (lxm_id+1) num_lxms
  in List.map (fun i -> posting_arys.(i-first_lxm_id)) super_ids

(* 
 * Important: start with HIGHEST lxm_ids first, so their posting_arys get set,
 * and their vals can be used by lower lxm_ids.
 *)
let posting_ary_for_lxm_id (lxm_id:int) (posting_arys:posting_ary array) (first_lxm_id:int) : posting_ary =
  let spas = super_posting_arys lxm_id posting_arys first_lxm_id in
    Ord_int_ary.merge (Int_ary_tbl.get_exn tbl lxm_id :: spas)

(* for a range of lexeme IDs *)
let posting_arys (first_lxm_id:int) (after_lxm_id:int) : posting_ary array =
  let result = Array.make (after_lxm_id - first_lxm_id) empty_posting_ary in
    for i = (after_lxm_id-1) downto first_lxm_id do
      result.(i-first_lxm_id) <- posting_ary_for_lxm_id i result first_lxm_id
    done;
    result

module Tbl = Tbl_mkr.Make(Idx_file.IntAryData)
	
let create_one_shard (ch_code:int) : unit =
  match Lexicon.bound_ids lexicon ch_code with
    | None -> ()
    | Some (first_lxm_id, after_lxm_id) ->
  let shard_mkr = Tbl.create dir_name (in_tbl_name "ids" ch_code) in
	  Array.iter
	    (fun posting_ary -> Tbl.push shard_mkr posting_ary)
	    (posting_arys first_lxm_id after_lxm_id);
	  Tbl.close shard_mkr

let mkdir_p (name:string) : unit =
  try
    Unix.mkdir name 0o755
  with
    Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let with_tbl_mkr (dir:string) (name:string) (f:Tbl.t -> 'a) : 'a =
  let out_tbl = Tbl.create dir (out_tbl_name name) in
	  let res = f out_tbl in
      Tbl.close out_tbl;
      res

(* kind: ids *)
let combine_shards () : unit =
  (* make directory *)
  let big_dir_name = Filename.concat dir_name "big"
  in
    mkdir_p big_dir_name;

    (* for each shard (0..255), push ids *)
    Tbl.with_tbl big_dir_name (out_tbl_name "ids")
      (fun out_tbl ->
	 for ch_code = 0 to 255 do
	   if file_exists_p dir_name (data_file_name "ids" ch_code) then
	     let in_tbl =
	       Int_ary_tbl.open_tbl dir_name (in_tbl_name "ids" ch_code)
	     in Int_ary_tbl.iter (Tbl.push out_tbl) in_tbl
	 done)
	
(*--- jocaml minion ---*)

(* port.  both master & minions listen here. *)
let addr = Unix.ADDR_INET (Join.Site.get_local_addr(), 54321)

(* -> 1 Join.chan *)
(* sends this Join.chan as SASE along w/ request for task. *)
let minion_chan request_job =
  def do_job(ch_code, job_done) =
    create_one_shard ch_code;
    job_done() & request_job(do_job)
  in do_job


(* Find Join.chan 'request_job' w/ which to comm w/ master.
 * Use it to create 'do_job' chan.
 * Listen for failure of comm port [i.e. master quits],
 * but otherwise don't ever quit.
 *)
let minion_main () =
  try
    let request_job = Join.Ns.lookup (Join.Ns.there addr) "register" in
      spawn request_job(minion_chan request_job);
      Join.Site.at_fail (Join.Site.there addr) begin
	def quit() = (* print_endline "minion exiting."; *) exit 0; 0 in quit
      end;
      def wait() & forever() = reply to wait
      in wait()
  with _ -> ()

(*--- jocaml master ---*)

(* -> 2 Join.chan *)
let master_chans () =
  (* parcel out, maybe. *)
  def job_requested(minion) & next_ch(ch_code) & in_process(n) =
    if ch_code > 255 then
      all_done() & in_process(n)
    else
      minion(ch_code, job_done) & next_ch(ch_code+1) & in_process(n+1)
  (* a job finishes. *)
  or job_done() & in_process(n) = in_process(n-1)
  (* everything's done. *)
  or in_process(0) & all_done() & wait() = reply to wait
  in
    (* init. *)
    spawn in_process(0) & next_ch(0);
    (* return Join.chans. *)
    (job_requested, wait)

let master_main num_minions =
  let (job_requested, wait) = master_chans() in
    (* publish Join.chan to minions as "register".
         master's name for it: job_requested.
         minion's name for it: request_job.   *)
    Join.Ns.register Join.Ns.here "register" job_requested;
    Join.Site.listen addr;
    (* fork minions. *)
    for i = 1 to num_minions do
      match Unix.fork() with
        | 0 -> Unix.execv Sys.argv.(0)
	    [| Sys.argv.(0); Sys.argv.(1); Sys.argv.(2); "--minion" |]
        | _ -> ()
    done;
    (* wait for all work to complete. *)
    wait();
    combine_shards ()

let _ =
  try
    match Sys.argv.(3) with
      | "--minion" -> minion_main()
      | num_str ->
	  let num_minions = max (int_of_string num_str) 1 in  (* >= 1 *)
	    master_main num_minions
  with _ -> master_main 10
