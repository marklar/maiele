(* https://godirepo.camlcity.org/svn/lib-ocamlnet2/trunk/code/examples/camlbox/manymult.ml *)

(* N subprocesses solve matrix multiplications 

   Each child has a camlbox where it receives matrices (of type
   float array array) to multiply. The master also has a camlbox where
   it receives the results.

   For simplicity of the example we only support square matrices.

   Invoke like:

   ./manymult <num_matrices> <matrix_size> <num_workers>
*)

open Printf
open Netcamlbox

type request =
    { req_id : int
    ; size : int
    ; left : float array array
    ; right : float array array
    }

type response =
    { resp_id : int
    ; worker_id : int
    ; result : float array array
    }

type task =
    { request : request
    ; mutable response_opt : response option
    }

type worker_arg =
    [ `Multiply of request
    | `Terminate
    ]

let create_tasks (n:int) (size:int) : task array =
  let make_task (id:int) : task =
    let req =
      let random_float_ary () = Array.init size (fun _ -> Random.float 1.) in
	{ req_id = id
	; size = size
	; left  = random_ary()
	; right = random_ary()
	}
    in
      { request = req 
      ; response_opt = None
      }
  in Array.init n make_task

(* matrices are SQUARE *)    
let multiply (worker_id:int) (req:request) : response =
  let max = req.size-1 in
  let r = Array.make_matrix req.size req.size 0.0 in
    for row = 0 to max do
      for col = 0 to max do
	let s = ref 0.0 in
	  for j = 0 to max do
	    s := !s +. req.left.(j).(col) *. req.right.(row).(j)
	  done;
	  r.(row).(col) <- !s
      done
    done;
    { resp_id = req.req_id
    ; worker_id = worker_id
    ; result = r
    }

(* Event loop.
 * Receives messages (new_msgs).
 * Processes each (either to quit, or to multiply matrices).
 * If multiply, send response back to client.
 * If quit, then exit.
 *)
let run_worker (worker_id:int) (worker_box:camlbox) (mfd:Unix.file_descr) =
  let client:camlbox_sender = camlbox_sender_of_fd mfd
  and cont = ref true in
  let process_msg (msg_num:int) =
    (* from msg number, we can get a REFERENCE to the contents. *)
    let (msg:worker_arg ref) = camlbox_get worker_box msg_num in
      match !msg with
	| `Multiply req ->
	    (* No necessary to copy req (msg) from box.
	       can just use its contents to generate a brand-new
	       response struct. *)
	    (* req contains 2 matrices.  multiply them. *)
	    let resp:response = multiply worker_id req in
	      (* req no longer needed.  delete its msg. *)
	      camlbox_delete worker_box msg_num;
	      (* send resp to client. *)
	      camlbox_send client resp
	| `Terminate ->
	    cont := false
  in
    while !cont do
      let (new_msgs:int list) = camlbox_wait worker_box in
	List.iter process_msg new_msgs
    done;
    exit 0

(* Camlbox capacity *)
let num_slots = 2           (* >1 so that "always" has something to do. *)
let msg_size = 1024 * 1024  (* 1M fixed size *)

let master_name = "camlbox_" ^ string_of_int (Unix.getpid())

(* With a file descriptor (fd) for a camlbox,
   we can pass that value to each fork (worker).
   Worker can use that fd to create a sender.
   (Why not export the *name* of the master's camlbox instead?
*)
let create_master_box (num_workers:int) : (camlbox * Unix.file_descr) =
  let box = create_camlbox master_name (num_slots*num_workers) msg_size in
  let fd = camlbox_fd master_name in
    (* why remove its global name? *)
    unlink_camlbox master_name;
    (box, fd)

type worker = (camlbox,         (* DON'T NEED THIS.  (Except to prevent its being GC-ed?) *)
	       camlbox_sender,  (* Endpoint for sending to this box (worker). *)
	       int ref)         (* Num slots currently free *)

let create_workers (num_workers:int) (master_fd:Unix.file_descr) : (worker array * int list) =

  (* Create worker boxes in the master process.
     Get a sender on each one.
     Fork a process for each worker, telling it:
       - which is its box, and
       - the file descr of the master's camlbox, so worker can create sender to master.
     At this point, the master doesn't need the worker boxes anymore.
     (Should it keep references to them, so they're not GC-ed?  Can they be GC-ed?)
  *)
  let (worker_boxes:worker array) =
    let create_one_worker (id:int) =
      let (box, sender) =
	(* Why do we use this particular name to get a sender to worker?
	   Is there some reason it should (partially) match with the
	   *unused* name of the master's camlbox?
	   (The workers don't use its name -- they use its file descr.)
	*)
	let name   = sprintf "%s_%d" master_name id in
	let box    = create_camlbox name num_slots msg_size in
	let sender = camlbox_sender name in
	  (* Remove global name of this worker camlbox.
	     No-one else will be able to get a sender to it.
	     We wanted name only long enough to get a sender to it.
	  *)
	  unlink_camlbox name;
	  (box, sender)
      in (box, sender, ref num_slots)
    in Array.init num_workers create_one_worker

  and pids = ref [] in
  let fork_worker (id:int) (box,_,_) =
    match Unix.fork() with
      | 0   -> run_worker id box master_fd
      | pid -> pids := pid :: !pids
  in
    Array.iteri fork_worker worker_boxes;
    (* At this point, no longer need the camlboxes themselves.
       just (for each):   - sender
                          - num_free_slots
       Perhaps we shouldn't return 'workers', but just those two data.
    *)
    (worker_boxes, !pids)

(* Create master box.
 * Create workers, saving their boxes and pids.
 *)  
let prepare (num_workers:int) : (camlbox * worker array * int list) =
  let (master_box, master_fd) =
    create_master_box num_workers in
  let (worker_boxes, pids) =
    create_workers num_workers master_fd
  in (master_box, worker_boxes, pids)

(* [0..n] *)
let list_0_to_n (n:int) : int list =
  let rec loop acc = function
    | 0  -> 0 :: acc
    | n' -> loop (n'::acc) (n'-1)
  in loop [] n

(* Each must have its response set. *)
let verify_all_tasks : task array -> unit =
  Array.iter (fun t -> assert (t.response_opt <> None))

(* Get sender to each worker.  Send 'terminate' msg. *)
let terminate_children : worker array -> unit =
  Array.iter (fun (_, sender, _) -> camlbox_send sender (ref `Terminate))

(* Waitpid for each child. *)  
let collect_children : int list -> unit =
  List.iter (fun pid -> ignore (Unix.waitpid [] pid))
	
(* Get response.  Copy it.  Delete from inbox. *)
let get_response (box:camlbox) (idx:int) : response =
  let msg = camlbox_get box idx in
    (* Copy contents. *)
  let msg' = { resp_id = msg.resp_id   (* same as task idx *)
	     ; worker_id = msg.worker_id
	     ; result = Array.map (fun c -> Array.copy c) msg.result
	     }
  in
    camlbox_delete box idx;  (* Delete it. *)
    msg'

(* updates ids in-place. *)
let dequeue_task (tasks:task array) (ids:int list ref) : task =
  let id = List.hd !ids in
    ids := List.tl !ids;
    tasks.(id)

let run_master (tasks:task array) (num_workers:int)
    (master_box:camlbox, workers:worker array, pids:int list) : unit =
  (* Loop.  MAKE THIS FUNCTIONAL. *)
  let num_responses_left = ref num_tasks in
  let (task_ids_to_do:int list ref) = ref (list_0_to_n (Array.length tasks)) in
    (* TFS helps us know whether to dequeue a task. *)
  let total_free_slots = ref (num_workers * num_slots)
  in
    (* While there's still work not fully account for... *)
    while !num_responses_left > 0 do
      (* Favor sending out tasks to receiving responses. *)
      (* Have something to do, and someone we can tell to do it... *)
      if !total_free_slots > 0 && !task_ids_to_do <> [] then (
	(* Submit new request.  First, dequeue a task. *)
	let task = dequeue_task tasks task_ids_to_do in
	  (* Find a worker to receive task. *)
	let is_submitted = ref false in
	  Array.iter
	    (fun (_, sender, num_free) ->
	       (* Task still not doled out, and worker has room.  Found one. *)
	       if not !is_submitted && !num_free > 0 then (
		 (* Use the 'sender' we created for this worker's box. *)
		 camlbox_send sender (ref (`Multiply task.request));
		 (* Update state. *)
		 decr num_free;
		 decr total_free_slots;
		 is_submitted := true
	       )
	    )
	    workers;
	  assert (!is_submitted)
      )
	(* All workers busy (or there's nothing left to do).
	   WAIT for response: *)
      else (
	(* Get idxs of responses.
	   (Don't map them to responses.  We still need the idxs.) *)
	let idx_list = camlbox_wait master_box in
	  List.iter
	    (fun idx ->
	       let msg = get_response master_box idx in
		 (* UPDATE TASK w/ response. *)
		 tasks.( msg.resp_id ).response_opt <- Some msg;
		 (* Update worker's availability. *)
		 let (_, _, num_free) = workers.( msg.worker_id ) in
		   incr num_free;

		   (* Update state. *)
		   incr total_free_slots;
		   decr num_responses_left;
	    )
	    idx_list
      )
    done;
    verify_all_tasks tasks;
    terminate_children workers;
    collect_children pids

let main() =
  let iarg i = int_of_string Sys.argv.(i) in
  let (num_matrices, matrix_size, num_workers) = (iarg 1, iarg 2, iarg 3) in
    printf "Forking children...\n%!";
    let (master, workers, pids) = prepare num_workers in

      printf "Creating tasks...\n%!";
      let (tasks:task array) = create_tasks num_matrices matrix_size in

	printf "Performing tasks...\n%!";
	let t0 = Unix.gettimeofday() in

	  run_master tasks num_workers (master, workers, pids);
	  let t1 = Unix.gettimeofday() in
	    printf "t = %f\n%!" (t1-.t0)

let () =
  main()
