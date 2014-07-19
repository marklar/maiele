open Printf;;  open Util

let join_port = 10499

let lexemes_from_cmd_args () : string list =
  Array.to_list (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1))

let query_str_from_cmd_line () : string =
  String.concat " " (lexemes_from_cmd_args())

let register_chan_name = "register"

(*--- main --- *)

let offset = 0
let limit = 10
let in_stock = false
let bop_p = false
let dns = Cfg.all_domain_names  (* ["games", "tablets"] *)
(* let dns = ["games"] *)

(* N.B. This is here so we can time how long updating "in_stock" takes,
   without the overhead of setting up the DB connection (~3ms). *)
let _ = Dbd.get_dbd()

let log (str:string) (duration:float) : unit =
  printf "%s\tduration:%f\t%s\n" (formatted_localtime()) duration str

let parallel () =
  match Sys.argv.(1) with
    | "--minion" ->
	Jo_minion.main join_port register_chan_name
    | _ ->
	let json =
	  let search =
	    time_it "create minions"
	      (fun () ->
		 Jo_master.parallel_search_fun join_port register_chan_name log
	      )
	  and query_str = query_str_from_cmd_line() in
	    Unix.sleep 1;
	    time_it "all domains"
	      (fun () ->
		 search query_str dns 5 (*FIXME: show_size*) offset limit in_stock bop_p
	      )
	in print_endline json

let serial () =
  let json =
    let search = Jo_master.serial_search_fun log
    and query_str = query_str_from_cmd_line() in
      time_it "all domains"
	(fun () -> search query_str dns 5 (*FIXME: show_size*) offset limit in_stock bop_p)
  in print_endline json
  

let _ =
  (*-- signals --*)
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _signum -> log "Caught SIGINT." 0.; exit 0) );
  Sys.set_signal Sys.sigsegv
    (Sys.Signal_handle
       (fun _signum -> log "Caught SIGSEGV.  (Stack overflow?)" 0.; exit 1) );
  at_exit (fun () -> log "Exiting" 0.);
  (*-- signals --*)
  
  serial()
