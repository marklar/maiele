open Http_types;;  open Pcre;;  open Printf;;  open Util

type json = string
type http_response_code = int

let send_email (req:Http_types.request) (port:int) : unit =
  let body = sprintf "hostname: %s\nport:%d\nuri: %s\ntimeout_secs: %f\n"
    (Unix.gethostname())
    port
    req#uri
    Cfg.jo_timeout_secs
  in Netsendmail.sendmail (Netsendmail.compose ~from_addr:("LSD", "mark@glyde.com") 
			     ~to_addrs:Cfg.email_addresses
			     ~subject:"LSD exited"
			     body)

(*
 * Daemon Specification: http://upsilon.cc/~zack/hacking/software/ocaml-http/html/Http_types.html
 * exn_handler : (exn -> Pervasives.out_channel -> unit) option;
 * The callback is meant to perform some clean up actions, like releasing global mutexes in `Thread mode
 *)
let handle_exceptions_in f (req:Http_types.request) (port:int) (log:Logger.t) : (http_response_code * json) =
  match Jo_util.timeout Cfg.jo_timeout_secs
    (fun () ->
       try (200, sprintf "{\"success\":true, \"results\":\n%s}\n" (f()))
       with exc ->
	 let (code, str) = match exc with
	   | Request.Invalid_param (name, value) ->
	       (400, sprintf "Invalid value for argument '%s': '%s'." name value)
	   | Request.Invalid_path p ->
	       (400, sprintf "Invalid path: '%s'." p)
	   | Request.Http_method_unsupported m ->
	       (405, sprintf "HTTP method upsupported: '%s'." m)
	   | Jo_master.Timeout ->
	       (408, "Timeout.")    (* not currently in use *)
	   | _ ->
	       (500, "Server error.")
	 in
	   log (sprintf "ERROR:%d - %s" code str) 0.;
	   (code, sprintf "{\"success\":false, \"exception\":\"%s\"}\n" str)  )
  with
    | Some result -> result
    | None ->
	log "timed out!" Cfg.jo_timeout_secs;
	send_email req port;
	exit 3

let base_headers = [ ("Content-Type", "application/json; charset=utf-8") ]
let expiry_secs = 1. *. 60. *. 60.  (* 1 hour. *)

let expiry_str () =
  let tm = Unix.gmtime (Unix.time() +. expiry_secs) in
    Time.format_tm "%a, %d %b %Y %X GMT" tm  (*annexlib*)

let callback_fun (search:Jo_master.search_fun) (log:Logger.t) (port:int)
    : (Http_types.request -> out_channel -> unit) =
  let callback (req:Http_types.request) (out_chan:out_channel) : unit =
    let (secs, _) = time_and_res 
      (fun () ->
	 let (code, body) = handle_exceptions_in
	   (fun () -> Controller.response_body req search) req port log
	 in
	 let headers =
	   if code == 200 && not $ Request.bool_param req "in_stock" then
	     ("Expires", expiry_str()) :: base_headers
	   else base_headers
	 in
	 let response_body = 
	   try 
	     let jsonp = Request.int_param req "jsonp"  (* raises if 'jsonp' req arg is absent *)
	     in sprintf "Glyde.jsonp_xhr[%d](%s)\n" jsonp body
	   with _ -> body
	 in
	   Http_daemon.respond
	      ~code:(`Code 200)  (* ALWAYS SEND 200. *)
	      ~headers
	      ~body:(Request.to_utf8 response_body)
	      out_chan  (* Send diff HTTP code? *)
      )
    in
      log req#uri secs;
      Gc.compact()
  in callback

(*
 * Daemon specification.  Mode:
 *   `Single - all requests handled by same process.
 *   `Thread - each request handled by a new thread.  Doesn't work at all.
 *   `Fork (default) - each request handled by a new child process.  Doesn't play nice w/ JoCaml.
*)
let start_daemon (port:int) (search:Jo_master.search_fun) (log:Logger.t) : unit =
  let spec = { Http_daemon.default_spec with
		   callback   = callback_fun search log port
		 ; port       = port
		 ; timeout    = Some 3   (* seconds *)
		 ; mode       = `Single  (* JoCaml-friendly. *)
		 ; auto_close = true     (* otherwise (default): keeps connection alive *)
	     }
  in Http_daemon.main spec


let register_chan_name = "register"
let http_join_delta = 20  (* FixMe: make cfg-able *)

(* master: lsd <http_port>
 * minion: lsd --minion <join_port>
 *)

let _ =
  match Sys.argv.(1) with
    | "--minion" ->
	let join_port = int_of_string Sys.argv.(2) in
	  Jo_minion.main join_port register_chan_name
    | num_str ->
	let http_port = int_of_string num_str in
	let log = Logger.create $ sprintf "lsd.%d" http_port in

	  (*-- signals --*)
	  Sys.set_signal Sys.sigint
	    (Sys.Signal_handle
	       (fun _signum -> log "Caught SIGINT." 0.; exit 0) );
	  Sys.set_signal Sys.sigterm
	    (Sys.Signal_handle
	       (fun _signum -> log "Caught SIGTERM." 0.; exit 1) );
	  Sys.set_signal Sys.sigsegv
	    (Sys.Signal_handle
	       (fun _signum -> log "Caught SIGSEGV.  (Stack overflow?)" 0.; exit 2) );
	  at_exit
	    (fun () ->
	       log "Closing index files." 0.;
	       Searcher.close();
	       log "Exiting." 0.
	    );
	  (*-- signals --*)
	  
	  let (mode_name, search) =
	    if Cfg.jo_parallel_p then
	      let join_port = http_port + http_join_delta in
		("PARALLEL", Jo_master.parallel_search_fun join_port register_chan_name log)
	    else
	      ("SEQUENTIAL", Jo_master.serial_search_fun log)
	  in
	    Logger.create_pid_file $ sprintf "lsd.%d" http_port;
	    log (sprintf "--- STARTING daemon: %s mode ---" mode_name) 0.;
	    start_daemon http_port search log
