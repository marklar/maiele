(**
   Goal: Create a new results table from an existing one,
   adding 'faux' records to it.

   Do *after* normalizing pops.

   1. hashtbl: title_2_res_ids
   2. list: falsos
       - iterate keys of title_only...
       - if >1 res_id, create record (w/ next available pop_rank)
       - sort them by sort_val (and then title)
   3. new res table:
       - iterate through reals, inserting falsos (based on sort order)
*)

open Printf;;  open Util;;  open ExtList

type falso = { sort_val   : int
	     ; pop        : int
	     ; text       : string
	     ; target_ids : Int_ary.t
	     }

let falso_cmp (a:falso) (b:falso) : int =
  compare (a.sort_val, a.text) (b.sort_val, b.text)

let make_falso (sv:int) (p:int) (s:string) (ids:Int_ary.t) : falso =
  { sort_val   = sv
  ; pop        = p
  ; text       = s
  ; target_ids = ids
  }

(* cfg: begin *)
let dir_name =
  try Sys.argv.(1)
  with _ -> failwith $ sprintf "usage: %s <dir_name>" Sys.argv.(0)
(* cfg: end *)

(* what if res_tbl lacks some partitions? *)
let orig_pop_col        = Int_ary.from_file    dir_name "res.pop.data"
let orig_text_col       = Str_tbl.open_tbl     dir_name "res.text"
let orig_target_ids_col = Int_ary_tbl.open_tbl dir_name "res.target_ids"
let orig_title_only_col = Str_tbl.open_tbl     dir_name "res.title_only"
let orig_sort_val_col   = Int_ary.from_file    dir_name "res.sort_val.data"

let est_size = 50000 (*games*)

let create_title_2_res_ids () : (string, int list) Hashtbl.t =
  let h = Hashtbl.create est_size in
  let cons_val i s = 
    if s <> "" then
      try
	Hashtbl.replace h s (i :: (Hashtbl.find h s))
      with Not_found ->
	Hashtbl.add h s [i]
    else
      ()
  in
    Str_tbl.iteri cons_val orig_title_only_col;
    h

(*
  We have Hashtbl: title => [res_id]
  For those w/ >1 res_id, create a falso result.
*)
let create_list_of_falsos (title_2_res_ids:(string, int list) Hashtbl.t)
    : falso list =
  let falso_pop = ref (Int_ary.max_val_unordered orig_pop_col) in
  let unsorted_falsos = Hashtbl.fold   (* res_ids: rev order *)
    (fun title res_ids falsos ->
       match res_ids with
	 | [] | _ :: [] -> falsos  (* <2x -> don't create falso *)
	 | _ ->
	     incr falso_pop;
	     let target_ids = Ord_int_ary.merge
	       (List.map (Int_ary_tbl.get_exn orig_target_ids_col) res_ids)
	     and sort_val =
	       (int_of_string |> Doc_lexer.sort_len_str |>
		    Doc_lexer.canonicalize_str |> Entities.down) title
	     in (make_falso sort_val !falso_pop title target_ids) :: falsos
    )
    title_2_res_ids []
  in List.sort ~cmp:falso_cmp unsorted_falsos

module RT = Faux_result_tbl_mkr

let create_new_result_partitions (all_falsos:falso list) : unit =
  let mkr = RT.create dir_name "new.res" in

  let create_real (id:int) : unit =
    let pop = Int_ary.get orig_pop_col (id-1)
    and text = Str_tbl.get_exn orig_text_col id
    and target_ids = Int_ary_tbl.get_exn orig_target_ids_col id in
      RT.push mkr pop target_ids text false
  in
  (*
    Iteri through Str_tbl 'orig_title_only_col'.
    Before creating a "real" record with it,
    read off all "falsos" that should go before it.
  *)
  let falsos = ref all_falsos in
  let rec loop (idx:int) (sort_val:int) : unit =
    let id = idx + 1 in
    let text = Str_tbl.get_exn orig_text_col id in
      (* 'consume' (i.e. create records for) some falsos;
	 remember which remain. *)
      falsos :=
	List.dropwhile 
	  (fun f ->
	    match compare (sort_val, text) (f.sort_val, f.text) with
	      | -1 -> false
	      |  _ ->
		   RT.push mkr f.pop f.target_ids f.text true;  (*side-effect*)
		   true
	  )
	  !falsos;
      (* create a real record. *)
      create_real id
  in
    Int_ary.iteri loop orig_sort_val_col;
    RT.close mkr

let _ =
  let title_2_res_ids = create_title_2_res_ids () in
  let falsos = create_list_of_falsos title_2_res_ids in
    create_new_result_partitions falsos
