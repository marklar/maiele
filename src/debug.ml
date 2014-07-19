open Util;;  open Printf

(* create/truncate file, return channel *)
let debug_log_out_chan = open_out "./debug.log"
  (* close_out oc;  *)               (* flush and close the channel *)

(* write and flush *)
let log (str:string) : unit =
  let tm_str = formatted_gmtime() in
    fprintf debug_log_out_chan "%s\t%s\n" tm_str str;
    flush debug_log_out_chan

(* IO *)
let log_time (str:string) (thunk:unit -> 'a) : 'a =
  let (secs,r) = time_and_res thunk in
    log (sprintf "%s: %f sec" str secs);  r
