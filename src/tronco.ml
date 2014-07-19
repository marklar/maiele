open ExtList;; open Pcre;; open Printf;; open Util

(*

TARGET IDS:
  2009-09-23 23:55:43     duration:0.000306       /maiele/target_ids.js?domain=games&id=2232
  AFTER search.  To create lineup.

RESULTS:
  2009-09-23 23:56:49     duration:0.029999       /maiele/results.js?query=and%20big%20li%20pig&limit=20&in_stock=true
  SEARCH.  Either buy- or sell-side.
  (In this case, buy, because 'in_stock=true'.)

RESULTS FOR TARGET:
  2009-09-23 23:57:15     duration:0.000417       /maiele/results_for_target.js?domain=games&target_id=7904294
  For Virtual Shelf.

*)

let query_re = regexp "query=([^&]*)"
let lsd_log_re = regexp "^lsd\\..*\\.log"  (* may have additional extensions. *)
let monit_query = "foo"

(* let t_secs (s:string) : int = *)

let logfile_names (dir_name:string) : string list =
  List.filter (pmatch ~rex:lsd_log_re) (File.names_in_dir dir_name)
  
let queries_from_lines (lines:string Enum.t) : string Enum.t =
  let matches_enum =
    Enum.map
      (fun ms -> ms.(1))
      (Ext_pcre.matches_from_strs_enum query_re lines)
  in Enum.map Ext_pcre.replace_hexes (Enum.filter ((<>) monit_query) matches_enum)

let queries_from_file (file_name:string) : string Enum.t =
  queries_from_lines (File.input_lines file_name)
  
let queries_from_dir (dir_name:string) : string Enum.t list =
  let res = List.map
    (fun lf -> queries_from_file (dir_name ^ "/" ^ lf))
    (logfile_names dir_name)
  in
    (* print_endline "here"; *)
    res

let dir_name = "log/fe01"

let _ =
  List.iter (Enum.iter print_endline) (queries_from_dir dir_name)
