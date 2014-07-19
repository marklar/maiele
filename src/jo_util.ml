let get_join_addr (port_num:int) : Unix.sockaddr =
  Unix.ADDR_INET (Join.Site.get_local_addr(), port_num)

(*
 * We kick off two (JoCaml) processes:
 *   1. calculate our desired result
 *   2. fire timeout after X seconds
 * and wait (synchronously) for whichever finishes first.
 * 
 * If we're waiting and
 *   - our calculation finishes, return result (wrapped in an option).
 *   - the timeout fires, return None.
 *)
let timeout (secs:float) (f:unit -> 'a) : 'a option =
  def wait() & finished(r) = reply Some r to wait
  or  wait() & timeout()   = reply None   to wait
  in
    spawn begin
      (* 2 simultaneous actions.
	 If timeout() before f() finishes, then f() will continue to run,
	 but we won't wait for it. *)
      finished(f()) &
	begin Thread.delay secs; timeout() end
    end;
    (* Need wait() so as to reply to it. *)
    wait()


(* we define a infinite loop that computes a function f at each iteration and
   that can be killed between two iterations. *)
(*
exception Killed
let create_loop f =
  (* internal status: ok, killed. -- at each moment, a msg pending on either one or other. *)
  (* control from outside: start (sync), kill (async). *)
  def ok()     & run()  = ok() & reply f(); run() to run
  or  killed() & run()  = reply raise Killed to run
  or  ok()     & kill() = killed()
  in
    let start () = spawn ok(); run() in
      start, kill
*)
(*
let run, kill =
  create_loop (fun () -> print_string "*"; Thread.delay 0.01)
*)
