open ExtList

let id x = x

let ($) f a = f a

let compose f g i = f (g i)
let (|>) = compose

(* Are these too similar-looking? *)
let (<=>) (a:int) (b:int) = compare a b
  (** Convenient to have this be polymorphic,
      but we provide type hints as an aid(?) for performance. *)

let (<->) (i:int) ((lo:int), (hi:int)) : bool =  (* in? *)
  lo <= i && i <= hi

(*
  Not bi-directional - just low to high.
  Does not support steps.
*)
let range (low:int) (high:int) : int list =
  let rec loop acc hi =
    if hi < low then
      acc
    else
      loop (hi :: acc) (hi-1)
  in loop [] high

let zero_pad (width:int) (s:string) : string =
  match width - (String.length s) with
    | neg when neg < 0 ->
	invalid_arg (Printf.sprintf "width (%d) narrower than string ('%s')" width s)
    | pad_len ->
	(String.make pad_len '0') ^ s

let pad_str (n:int) (s:string) : string = 
  String.make (n - (String.length s)) ' '

let rjust (n:int) (s:string) : string =
  (pad_str n s) ^ s

let ljust (n:int) (s:string) : string =
  s ^ (pad_str n s)

(* regexps *)
let whitespace_beg_or_end_re = Pcre.regexp "(^\\s*|\\s*$)"

(* substitutions *)
let space_sub = Pcre.subst " "
let empty_sub = Pcre.subst ""

let strip (text: string) : string =
  Pcre.replace text ~rex: whitespace_beg_or_end_re ~itempl: empty_sub

let compact_whitespace : string -> string =
  strip |> Pcre.replace ~itempl: space_sub

(* superstr_p "foo" "foo"  => true *)
let superstr_p (str_a:string) (str_b:string) : bool =
  if str_a = str_b then
    true
  else
    let len_b = String.length str_b in
      len_b <= String.length str_a && str_b = String.sub str_a 0 len_b

let all_prefixes (s:string) : string list =
  let rec loop acc len =
    if len <= 0 then
      acc
    else
      loop (String.sub s 0 len :: acc) (len-1)
  in loop [] (String.length s)

(* "besprinkle" *)
(* warp (many parallel) & weft (single, bending continuous) *)
let split_all_and_join (orig:string) (sub:string) : string =
  let rec loop acc idx =
    if idx < 0 then
      acc
    else
      loop (String.sub orig idx 1 :: acc) (idx-1)
  in String.concat sub (loop [] (String.length orig - 1))

let string_from_chars (chs:char list) : string =  
  let res = String.create (List.length chs) in
  let rec loop chs idx : unit =
    match chs with
      | [] -> ()
      | hd :: tl ->
	  String.set res idx hd;
	  loop tl (idx+1)
  in loop chs 0;
    res

(**-- List --*)

(*
  first value for which option-creating function
  returns a Some.
  if none, raise Not_found.
*)
let rec find_map_exn (f:'a -> 'b option) (xs:'a list) : 'b =
  match xs with
    | [] -> raise Not_found
    | x::xs' -> match f x with
	| Some y -> y
	| None -> find_map_exn f xs'

(*
let foldl (f:'a -> 'a -> 'a) (list:'a list) : 'a =
  match list with
    | [] -> []
    | h :: t -> List.fold_left f h t
*)

(* Faster than sort. *)
let insert_into_sorted_list (x:'a) (xs:'a list) (cmp:'a -> 'a -> int) : 'a list =
  let rec loop fore = function
    | [] -> xs @ [x]
    | hd :: tl as aft ->
	match cmp x hd with
	  | -1 -> (List.rev fore) @ (x :: aft)
	  |  _ -> loop (hd :: fore) tl
  in loop [] xs

(*
  Insert x into xs.
  Best position: where it belongs, according to cmp.
  Worst position: n.
*)
let insert_into_top_n (n:int) (x:'a) (xs:'a list) (cmp:'a -> 'a -> int) : 'a list =
  let rec loop fore aft n' =
    if n' >= (n-1) then
      (List.rev fore) @ (x :: aft)
    else
      match aft with
	| [] -> xs @ [x]
	| hd :: tl as aft ->
	    match cmp x hd with
	      | -1 -> (List.rev fore) @ (x :: aft)
	      |  _ -> loop (hd :: fore) tl (n'+1)
  in loop [] xs 0

let rev_sort (cmp:'a -> 'a -> int) (xs:'a list) : 'a list =
  List.sort ~cmp:(fun a b -> cmp b a) xs

(*
  Insert subjs into objs using insert_into_top_n.
*)
let insert_multi_into_top_n (n:int) (subjs:'a list) (objs:'a list)
    (cmp:'a -> 'a -> int) : 'a list =
  let num = n - (List.length subjs) + 1 in
    List.fold_left
      (fun m subj -> insert_into_top_n num subj m cmp)
      objs
      (rev_sort cmp subjs)

(* check that off, limit >= 0 *)
let sub (list:'a list) (off:int) (lim:int) : 'a list =
  let max = off + lim in
  let rec loop n xs acc = match (n, xs) with
    | (_, [])                  -> acc                      (* nothing more to consume *)
    | (n, _) when n >= max     -> acc                      (* don't need any more *)
    | (n, _::xs') when n < off -> loop (n+1) xs' acc       (* not yet taking *)
    | (n, x::xs')              -> loop (n+1) xs' (x::acc)  (* taking, still need *)
  in List.rev $ loop 0 list []

(* Does NOT maintain sort order. *)
let uniq (list:'a list) : 'a list =
  let h = Hashtbl.create (List.length list) in
    List.iter (fun i -> Hashtbl.replace h i 1) list;
    Hashtbl.fold (fun k _ r -> k :: r) h []

(**-- Time --*)
let formatted_gmtime () =
  let tm = Unix.gmtime (Unix.time ()) in
    Time.format_tm "%a, %d %b %Y %X GMT" tm  (* annexlib *)

let formatted_localtime () =
  let tm = Unix.localtime (Unix.time ()) in
    Time.format_tm "%Y-%m-%d %X" tm  (* annexlib *)

let time_and_res (thunk:unit -> 'a) : (float * 'a) =
  let t  = Unix.gettimeofday () in
  let r  = thunk () in
  let t' = Unix.gettimeofday () in
    (t' -. t, r)

(* IO *)
let time_it (str:string) (thunk:unit -> 'a) : 'a =
  print_endline (str ^ "...");
  let (t, r) = time_and_res thunk in
    Printf.printf "%s: %f sec\n" str t;  flush stdout;
    r

(**-- File --*)

let tmp_file_name () : string = 
  string_of_int (int_of_float (100. *. (Unix.gettimeofday ())))

let rm_file (path:string) : unit =
  ignore (Unix.system (Printf.sprintf "rm -f %s" path))

let file_exists_p (dir_name:string) (file_name:string) : bool =
  let fn = Filename.concat dir_name file_name in
    try ignore (Unix.stat fn); true
    with _ -> false

let file_size (dir_name:string) (file_name:string) : int =
  let fn = Filename.concat dir_name file_name in
    try (Unix.stat fn).Unix.st_size
    with _ -> 0

(**-- parallelism --*)

(* due to: Thomas Fischbacher *)

(* The pipe() system call asks the OS to construct a new anonymous
   pipe object. This results in two new, opened file descriptors in
   the process: the read-only end of the pipe, and the write-only
   end. (The pipe ends appear to be normal, anonymous file
   descriptors, except that they have no ability to seek.)

   To avoid deadlock and exploit parallelism, the process with one or
   more new pipes will then, generally, call fork() to create new
   processes. Each process will then close the end(s) of the pipe that
   it will not be using before producing or consuming any
   data.
*)
(* -> future which collects result of || computation *)
let invoke (f : 'a -> 'b) x : unit -> 'b =
  
  (* Unix.pipe -> tuple *)
  let input, output = Unix.pipe () in

    (* fork once. *)
    match Unix.fork () with 
	
      | -1 -> (let v = f x in fun () -> v)       (* cannot fork? *)
	    
      (* CHILD process *)
      | 0 ->

          (* child output result. *)
          Unix.close input;
          let output = Unix.out_channel_of_descr output in
	    (* calc expr, marshal, send to parent. *)
            Marshal.to_channel output
	      (try `Res(f x)
	       with e -> `Exn e)
	      [];   (* []: no special Marshal flags *)
	        
            (* close comm *)
            close_out output; 
            exit 0 
	            
      (* PARENT process *)
      | pid ->

          (* parent awaits input. *)
          Unix.close output;
          let input = Unix.in_channel_of_descr input in
	    (*
	      create "future" to return.
	      child process executes and sends result.
	      this future collects the result awaiting it.
	    *)
            fun () -> 
              let v = Marshal.from_channel input in   (* block on receipt of msg from child. *)
		ignore (Unix.waitpid [] pid);         (* wait for child process to terminate... *)
		close_in input;                       (* ...so we know we can close comm *)
		match v with
		  | `Res x -> x 
		  | `Exn e -> raise e 
