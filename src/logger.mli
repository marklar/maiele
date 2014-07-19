
(** Logging *)
val create_log_file : string -> unit

(* ascending order? *)
type level = All | Debug | Info | Error | Warn | No_log

val set_level : level -> unit

val log   : string -> float -> unit
val debug : string -> float option -> unit
val info  : string -> float option -> unit
val error : string -> float option -> unit
val warn  : string -> float option -> unit
val duration : string -> (unit -> 'a) -> 'a

val flush : unit -> unit

(** PID files *)
val create_pid_file : string -> unit
val remove_pid_file : string -> unit
