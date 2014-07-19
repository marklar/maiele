open Util;;  open Printf

let catch_signals () : unit =
  let signal_handlers =
    [ (Sys.sigint,  0, "Caught SIGINT.")
    ; (Sys.sigterm, 1, "Caught SIGTERM.")
    ; (Sys.sigsegv, 2, "Caught SIGSEGV.  (Stack overflow?)")
    ]
  in List.iter
       (fun (signal, code, str) ->
	  Sys.set_signal signal $
	    Sys.Signal_handle (fun _signum -> Logger.log str 0.; exit code)
       )
       signal_handlers

let catch_exit () : unit =
  at_exit
    (fun () ->
       Logger.log "Closing index files." 0.;
       Searcher.close ();
       Logger.log "Exiting." 0.
    )

(* public *)

let set_up () : unit =
  catch_signals ();
  catch_exit ()
