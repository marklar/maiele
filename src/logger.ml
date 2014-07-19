open Printf;;  open Util

let mv_prev_log_file (filename:string) : unit =
  try   (* Time.format_tm: from annexlib *)
    let suffix = Time.format_tm "%Y%m%d%H%M%S" $ Unix.gmtime (Unix.time())
    in Unix.rename filename (sprintf "%s.%s" filename suffix)
  with _ -> ()

let create_out_chan (name:string) : out_channel =
  let fn = sprintf "./log/%s.log" name in
    mv_prev_log_file fn;
    open_out fn

(* by default, log to stderr *)
let log_out_chan:out_channel ref = ref stderr

type level = All | Debug | Info | Error | Warn | No_log

let level:level ref = ref Debug

let set_level (v:level) : unit =
  level := v

let create_log_file (name:string) : unit =
  log_out_chan := create_out_chan name

let write (str:string)
    (prefix_opt:string option) (duration_opt:float option) : unit =
  let str' = match prefix_opt with
    | None -> str
    | Some s -> s ^ ": " ^ str
  and duration_str = match duration_opt with
    | None -> ""
    | Some f -> sprintf "\tduration:%f" f
  in
    fprintf !log_out_chan
      "%s%s\t%s\n"
      (formatted_localtime()) duration_str str'
    (* ;
    flush !log_out_chan *)

let flush () : unit =
  flush !log_out_chan

(*
  Lacks prefix.  Includes duration.
*)
let log (str:string) (duration:float) : unit =
  write str None (Some duration)

(*
  Include prefix.  Optional duration.
*)
let debug (str:string) (duration:float option) : unit =
  match !level with
    | All | Debug ->
	write str (Some "DEBUG") duration
    | _ -> ()

let info (str:string) (duration:float option) : unit =
  match !level with
    | All | Debug | Info ->
	write str (Some "INFO") duration
    | _ -> ()

let error (str:string) (duration:float option) : unit =
  match !level with
    | All | Debug | Info | Error ->
	write str (Some "ERROR") duration
    | _ -> ()

let warn (str:string) (duration:float option) : unit =
  match !level with
    | No_log -> ()
    | _ ->
	write str (Some "WARN") duration

let duration (str:string) (f:unit -> 'a) : 'a =
  let (secs, res) = time_and_res f in
    write str None (Some secs);
    res

(** PID file *)

let pid_file_name (name:string) : string =
  sprintf "./log/%s.pid" name

let remove_pid_file (name:string) : unit =
  try Unix.unlink $ pid_file_name name with _ -> ()

let create_pid_file (name:string) : unit =
  (* No need to rm first.  Truncated if exists. *)
  let out_chan = open_out $ pid_file_name name in
    fprintf out_chan "%d" $ Unix.getpid ();
    close_out out_chan
  
