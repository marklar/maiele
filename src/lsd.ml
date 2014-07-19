
open Http_types;;  open Pcre;;  open Printf
open Util;;  open Logger

(*
 * Daemon Specification:
 *   http://upsilon.cc/~zack/hacking/software/ocaml-http/html/Http_types.html
 * exn_handler : (exn -> Pervasives.out_channel -> unit) option;
 * The callback is meant to perform some clean up actions,
 * like releasing global mutexes in `Thread mode.
 *)
type http_response_code = int
let handle_exceptions_in (format:Request.format) (f:unit -> string)
    : (http_response_code * string) =
  try
    let body = match format with
      | `Json ->
	  sprintf "{\"success\":true, \"results\":\n%s}\n" (f ())
      | `Xml  ->
	  sprintf "<?xml version=\"1.0\" encoding='UTF-8'?>\n<results>\n%s\n</results>" (f ())
      | `Html ->
	  (f ()) ^ "\n"
    in (200, body)
  with exc ->
    let (code, str) = match exc with
      | Request.Invalid_param (name, value) ->
	  (400, sprintf "Invalid value for arg '%s': '%s'." name value)
      | Request.Invalid_path p ->
	  (400, sprintf "Invalid path: '%s'." p)
      | Request.Http_method_unsupported m ->
	  (405, sprintf "HTTP method upsupported: '%s'." m)
      | e ->
	  info (sprintf "Unexpected exception : %s" (Printexc.to_string e)) None;
	  (* info (Printexc.get_backtrace ()) None; *)
	  (* Must be pre-enabled:
	       + Printexc.record_backtrace true, OR
               + set environment variable OCAMLRUNPARAM="b1"
	  *)
	  (500, "Server error.")
    in
      (* modify to match format *)
      info (sprintf "ERROR:%d - %s" code str) None;
      (code,
       sprintf "{\"success\":false, \"exception\":\"%s\"}\n" str)

(*
  HTTP headers.
*)
let one_hour = 1. *. 60. *. 60.
let one_day = one_hour *. 24.
let expiry_secs_in_stock = one_hour
let expiry_secs = one_day
let expires_header () =
  let tm = Unix.gmtime (Unix.time () +. expiry_secs_in_stock) in
    ("Expires", Time.format_tm (* from annexlib *) "%a, %d %b %Y %X GMT" tm)

let expires_header_in_stock () =
  let tm = Unix.gmtime (Unix.time () -. expiry_secs) in
    ("Expires", Time.format_tm (* from annexlib *) "%a, %d %b %Y %X GMT" tm)

let cache_control_header_in_stock () =
    ("Cache-Control", "private, no-cache, no-store, no-transform, must-revalidate, proxy-revalidate, max-age=0")

let content_type_header (req:Http_types.request) : (string * string) =
  let ct = match Request.format req with
    | `Json -> "application/json"
    | `Xml  -> "application/xml"
    | `Html -> "text/html"
  in
    ("Content-Type", sprintf "%s; charset=utf-8" ct)

(*
  Possibly add 'expires' header.  Only if:
    - successful (no error)
*)
let headers (req:Http_types.request) (code:int) : (string * string) list =
  content_type_header req ::
    if code = 200 then
      if Request.bool_param req "in_stock" then
        [expires_header_in_stock (); cache_control_header_in_stock ()]
      else
        [expires_header ()]
    else []

(*
  If query contains 'cookie' header with '_session_id',
  Logger.info it.
*)
let log_session_id_and_query (req:Http_types.request) : unit =
  try
    let (_, session_id) = 
      let pairs = Cookies.name_val_pairs $ req#header ~name:"cookie" in
	List.find (fun (n,v) -> n = "_session_id") pairs
    and query_str = Request.query_str req
    in      
      info (sprintf "_session_id:%s\tquery:%s" session_id query_str) None
  with _ -> ()

(*
  Handler for each request.
*)
type callback = Http_types.request -> out_channel -> unit

let callback_fun (search:Search_fun.t) : callback =
  let callback (req:Http_types.request) (out_chan:out_channel) : unit =

    let (secs, _) = time_and_res 
      (fun () ->
	 (* Printexc.record_backtrace true; *)
	 (*
	   Response code:
	   - we calc it only to determine 'expires' header.
	   - but we always actually send 200.
	 *)
	 let format = Request.format req in
	 let (code, body) = handle_exceptions_in
	   format
	   (fun () -> Controller.response_body format req search)
   in
    let response_body =
      match Request.jsonp req 0 with
        | 0 -> body
        | jsonp -> sprintf "Glyde.jsonp_xhr[%d](%s)\n" jsonp body
		 in Http_daemon.respond
		      ~code:(`Code 200)
		      ~headers:(headers req code)
		      ~body:(Request.to_utf8 response_body)
		      out_chan
	      )
	    in
	      log req#uri secs;
	      log_session_id_and_query req;
	      Logger.flush ();
	      Gc.compact ()  (* superstition *)
  in callback

(*
 * Daemon specification.  Mode:
 *   `Single - all requests handled by same process.
 *   `Thread - each request handled by a new thread.  Doesn't work at all.
 *   `Fork (default) - each request handled by a new child process.
 *      Doesn't play nice w/ JoCaml.
*)
let start_daemon (port:int) (search:Search_fun.t) : unit =
  let spec =
    { Http_daemon.default_spec with
	  callback   = callback_fun search
	; port       = port
	; timeout    = Some 3   (* seconds *)
	; mode       = `Single  (* JoCaml-friendly. *)
	; auto_close = true     (* default is false: holds connection *)
    }
  in Http_daemon.main spec


(* lsd <http_port> *)
let _ =
  let http_port = int_of_string Sys.argv.(1) in
  let name = sprintf "lsd.%d" http_port in
    Logger.create_log_file name;
    Logger.create_pid_file name;
    Logger.set_level Logger.Info;
    Event_handlers.set_up ();
    
    let (mode_name, search) =
      ("SEQUENTIAL", Search_fun.search)
    in
      info (sprintf "--- STARTING daemon: %s mode ---" mode_name) None;
      start_daemon http_port search
