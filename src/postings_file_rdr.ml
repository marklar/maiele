(*
  Postings file's lines:
      <lexeme>  \t  <result_id>

  Create Str_tbl of lexemes.
  Create Matrix (lexeme_id -> result_ids)
*)
open Pcre;;  open Printf;;  open Util;;  open ExtList

(*
  the data from one line.
*)
type posting =
    { lxm : string
    ; id  : int
    }

let tab_re = regexp "\\t"
let posting_from_line (line:string) : posting =
  match split ~rex:tab_re line with
    | [lxm; id] ->
	{ lxm = lxm
	; id  = int_of_string id
	}
    | _ ->
	failwith $
	  sprintf "Wrong format for line:\n\t%s." line
	  
(* 
   Arrays.  Lists too long -> seg fault!
   Create postings to put into matrix.
*)
let ids_for_lexeme (postings:posting list) : int array =
  (* make empty array *)
  let ids = Array.make (List.length postings) 0
  in
    (* fill array with posting data.
       iterate thru postings of lexeme.
       record res_id for each. *)
    List.iteri
      (fun i p -> ids.(i)  <- p.id)
      postings;
    (* sort array *)
    Array.fast_sort compare ids;
    ids
  

(* 
   write all IDs, regardless of how many.
*)
let write_postings (mtx:Matrix_mkr.t) (postings:posting list) : unit =
  match postings with
    | [] -> ()
    | p :: _ ->
	let ids = ids_for_lexeme postings
	in Matrix_mkr.push mtx ids p.lxm

let do_all (dir_name:string) (file_name:string) =
  let in_chan = open_in (Filename.concat dir_name file_name)
  and mtx = Matrix_mkr.create dir_name in
  let rec loop (postings:posting list) : unit =
    (*
      Do NOT wrap recursive calls inside 'try' blocks,
      as that causes the stack to grow out of hand,
      causing seg-fault.
      Instead, leave very tight scope around code that may raise.
    *)
    let line =  
      try Some (input_line in_chan) 
      with _ -> None 
    in match line with 
      | None -> 
	  (* write last lexeme. *) 
	  write_postings mtx postings 
      | Some s -> 
	  let p = posting_from_line s in 
	    match postings with 
	      | hd::_ when hd.lxm <> p.lxm -> 
		  (* novel lexeme.  write postings for prev. *) 
		  write_postings mtx postings; 
		  loop [p] 
	      | _ ->  
		  (* prev lexeme: 
		     - none 
		     - same as current *) 
		  loop (p::postings) 
  in
    loop [];
    Matrix_mkr.close mtx

let _ =
  let (dir_name, file_name) =
    try (Sys.argv.(1), Sys.argv.(2))
    with _ -> failwith (sprintf "posting: %s <dir_name> <file_name>" Sys.argv.(0))
  in do_all dir_name file_name
