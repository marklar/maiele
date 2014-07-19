open Printf;;  open Util

(**
   also, refactor Jo_master.  pull out search funs into separate module.  put Jo_lsd code into Jo_master.
   plan:
   each minion exposes a struct of two chans:
     - for receiving jobs to do.
     - for accepting kill command -- what should it do?
*)

(*-- This code runs inside master, to create minions. --*)

(* - Define how to collect minion Join.chans.
 * - Join.chan get() is where minions will report for work.
 * - Register that chan at the NameService, giving it the same name ("register")
 *   the minions will use to lookup that chan.
 *   (Chan is registered with NS _before_ the minions are even created, so their lookups won't fail.)
 * - Start listening on join_addr, awaiting the arrival of the minions.
 * -> Join.chan collect_minions; when called, it will return the minion Join.chans.
 * 
 * interaction points (chans) get(), need(), and res() are all HIDDEN.
 * only wait() is exposed.
 *)
let collect_minions_chan (join_port:int) (reg_chan_name:string) (num:int) =
  def get(m) & need(n) & res(ms) = res(m::ms) & need(n-1)
  or  wait() & need(0) & res(ms) = reply ms to wait
  in
    Join.Ns.register Join.Ns.here reg_chan_name get;
    Join.Site.listen (Jo_util.get_join_addr join_port);
    spawn res([]) & need(num);
    wait  (* -> chan *)

let start_processes (num:int) (join_port:int) : unit =
  let join_port_str = string_of_int join_port in
    for i = 1 to num do
      match Unix.fork() with
	| 0 -> Unix.execv Sys.argv.(0) [| Sys.argv.(0); "--minion"; join_port_str |]
	| _ -> ()
    done

(* - Create minions.
 * - Have them register w/ master.
 * -> Join.chan list: those minion Join.chans.
 *)
let create_n (num:int) (join_port:int) (reg_chan_name:string) = 
  (*  (Str_query_tree.t * float * (string * Result.t list)) Join.chan list = *)
  let collect_minions = collect_minions_chan join_port reg_chan_name num in
    start_processes num join_port;
    collect_minions()


(*-- This code runs in each minion. --*)

(*
 * -> Join.chan, which:
 *    - accepts tuple of params from master and executes search.
 *    - returns results to master via its job_done Join.chan.
 *)
let channel () =
  (* turn this tuple [type] into an actual type. *)
  def do_job(str_query_tree, domain_name, show_size, offset, limit, in_stock_p, job_done) =
    let (secs, results) = time_and_res 
      (fun () ->
	 try Searcher.results ~domain_name ~show_size ~offset ~limit ~in_stock_p str_query_tree
	 with _ -> [] )
    in job_done(str_query_tree, secs, (domain_name, results))
  in do_job

(*
 * Never returns, except when master dies.
 * Registers do_job Join.chan w/ master.
 *)
(* let main (join_addr:Unix.sockaddr) (master_chan_name:string) : unit = *)
let main (join_port:int) (master_chan_name:string) : unit =
  try
    let join_addr = Jo_util.get_join_addr join_port in
    let register = Join.Ns.lookup (Join.Ns.there join_addr) master_chan_name in
    let do_job = channel() in
      spawn register(do_job);
      Join.Site.at_fail (Join.Site.there join_addr)
        (def quit() = exit 0; 0 in quit);
      def wait() & forever() = reply to wait
    in wait()
  with _ -> ()

