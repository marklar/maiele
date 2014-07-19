(*
 * Take multiple logfiles.  Blend into one.
 * 
 * Each line starts w/ timestamp:
 *    2009-09-23 23:55:43
 * These are alpha-sortable.
 * 
 * Open multiple files.  Get a (string Enum.t) on each.
 * Use:
 *   Enum.peek e - to look ahead at next value.
 *   Enum.get e  - to consume next value.
 * 
 * 
 *)

open Pcre;;  open Printf

let lsd_log_re = regexp "^lsd\\..*\\.log"  (* may have additional extensions. *)

let logfile_names (dir_name:string) : string list =
  List.map
    (fun s -> sprintf "%s/%s" dir_name s)
    (List.filter (pmatch ~rex:lsd_log_re) (File.names_in_dir dir_name))

let logfile_enums (dir_name:string) : string Enum.t list =
  List.map File.input_lines (logfile_names dir_name)


type file_enum = { enum         : string Enum.t
		 ; mutable line : string option  (* to avoid continually re-seek-ing *)
		 }

(*
  given a list of file-line enumerators,
  -> function to Enum.iter over their lines in chron order.
*)
let create_next (enums:string Enum.t list) : (unit -> string) =
  let file_enums =
    List.map (fun e -> {enum = e; line = Enum.get e}) enums
  in
  let min_timestamp memo next : file_enum =
    match next.line with
      | None -> memo
      | Some nl -> match memo.line with
	  | None -> next
	  | Some ml ->
	      if nl < ml then
		next
	      else
		memo
  in
  let min_file_enum () : file_enum option =
    match file_enums with
      | [] -> None
      | h :: t -> match List.fold_left min_timestamp h t with
	  | {line = None} -> None
	  | fe -> Some fe
  in
  let next_line () : string =
    match min_file_enum () with
      | None -> raise Enum.No_more_elements
      | Some fe -> match fe.line with
	  | None -> raise Not_found  (* should never happen *)
	  | Some line -> 
	      fe.line <- Enum.get fe.enum;  (* mutate *)
	      line
  in next_line

    
let dir_name = "log/fe02"
let query_re = regexp "duration:(.*)\\t.*query=([^&]*)"

let _ =
  let enums = (logfile_enums dir_name) in
  let all_lines_enum = Enum.from (create_next enums) in 
  let matches_enum =
    Enum.filter_map
      ( fun ms ->
	  if ms.(2) = "foo" || ms.(2) = "" then
	    None
	  else
	    Some (ms.(1), Ext_pcre.replace_hexes ms.(2)) )
      (Ext_pcre.matches_from_strs_enum query_re all_lines_enum)
  in
  let i = ref 0
  and sum_duration = ref 0.
  and sum_length = ref 0
  in
    Enum.iter
      (fun (d, q) ->
	 sum_duration := !sum_duration +. (float_of_string d);
	 sum_length   := !sum_length   +  (String.length q);
	 incr i)
      matches_enum;
    printf "fir directory: %s\n" dir_name;
    printf "num queries: %d\n" !i;
    printf "ave duration: %f\n" (!sum_duration /. (float_of_int !i));
    printf "ave length: %f\n" ((float_of_int !sum_length) /. (float_of_int !i))
