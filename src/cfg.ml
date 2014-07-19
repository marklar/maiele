(* http://martin.jambon.free.fr/json-wheel-doc/index.html *)
open Json_type;;  open Json_type.Browse
open Util

(* Read .js file into "table".  (See bottom of file for example of JSON data.)
 * Used everywhere below in this file.
 * Also exported for benefit of Dbd module.
 *)
let json_tbl:(string, Json_type.t) Hashtbl.t =
  make_table (objekt (Json_io.load_json ~allow_comments:true "./cfg/.config.js"))

(*-- Verticals/Domains --*)
let all_buy_domain_names:string list =
  try list string (field json_tbl "buy_domains")
  with _ -> ["browse"; "games"; "ce"; "tablets"; "mp3s"; "phones"; "accessories"]

let all_sell_domain_names:string list =
  try list string (field json_tbl "sell_domains")
  with _ -> ["games"; "ce"; "tablets"; "mp3s"; "phones" ; "laptops"]

let domains_with_in_stock_info:string list =
  try list string (field json_tbl "domains_with_in_stock_info")
  with _ -> [ "games"; "games_products"; "ce"; "tablets"; "mp3s"; "phones"; "accessories"]
let is_in_stock_domain (name:string) : bool =
  List.mem name domains_with_in_stock_info

(* For Jo_lsd, specifically.
   Whether to run in parallel mode (v. sequential).
   This is unused in (non-JoCaml) Lsd.
*)
let jo_parallel_p:bool =
  try bool (field json_tbl "jo_parallel_p")
  with _ -> true

let jo_timeout_secs:float = 
  try float (field json_tbl "jo_timeout_secs")
  with _ -> 2.0

let glyde_storefront_name:string = 
  try string (field json_tbl "glyde_storefront_name")
  with _ -> "glyde"

let solr_host:string = 
  try string (field json_tbl "solr_host")
  with _ -> "andrew.glyde.com"

let solr_port:int =
  try int (field json_tbl "solr_port")
  with _ -> 8983

let lsync_frequency_secs:float =
  try float (field json_tbl "lsync_frequency_secs")
  with _ -> 30.

let browse_min_glus:int =
  try int (field json_tbl "browse_min_glus")
  with _ -> 1

(*  [("Mark", "mark@glyde.com")]  *)
let email_addresses:(string * string) list =
  let name_addr obj =
    let tbl = make_table (objekt obj) in
      (string (field tbl "name"),
       string (field tbl "address"))
  in list name_addr (field json_tbl "email_addresses")

(* Extract all "name" fields: ["tablets"; "games"; ...] *)
let all_domain_names:string list =
  let json_idx_ary = field json_tbl "indices"
  and name json_idx_obj = string (field (make_table (objekt json_idx_obj)) "name")
  in list name json_idx_ary

let idx_root_dir = string (field json_tbl "idx_root_dir")

(* Info from JSON config file.  Used to create Domains. *)
type index = { name : string
	     ; dir  : string
	     }

let idx_name (idx:index) = idx.name
let idx_dir  (idx:index) = idx.dir

let indices:index list =
  let create_index json_idx : index = 
    let obj = make_table (objekt json_idx) in
      { name = string (field obj "name")
      ; dir  = string (field obj "dir")
      } in
  let json_idx_ary = field json_tbl "indices" in
    list create_index json_idx_ary


(*
{"db_pwd":"",
 "db_host":"dev-db01",
 "verticals":["games","games_products"],
 "buy_domains":["browse","games",],
 "sell_domains":["games",],
 "glyde_storefront_name":"glyde",
 "db_port":58306,
 "solr_host":"andrew.glyde.com",
 "ports":[10444,10445],
 "db_name":"peeko_prod",
 "solr_port":8983,
 "parallel_p":true,
 "tag_db_name":"peeko_catalog_glu_tags",
 "lsync_frequency_secs":30.0,
 "timeout_secs":3.0,
 "db_user":"root",
 "idx_root_dir":"\/disk2\/home\/mark\/Work\/Maiele\/idx\/",
 "indices":[{"dir":"browse",
	     "name":"browse"},
	    {"dir":"games_platforms",
	     "name":"games_platforms"},
	    {"dir":"games",
	     "name":"games"},
	    {"dir":"games_ps2",
	     "name":"games_ps2"},
	    {"dir":"games_gamecube",
	     "name":"games_gamecube"},
	    {"dir":"games_ps3",
	     "name":"games_ps3"},
	    {"dir":"games_xbox360",
	     "name":"games_xbox360"},
	    {"dir":"games_ds",
	     "name":"games_ds"},
	    {"dir":"games_3ds",
	     "name":"games_3ds"},
	    {"dir":"charities",
	     "name":"charities"},
	    {"dir":"games_xbox",
	     "name":"games_xbox"},
	    {"dir":"games_wii",
	     "name":"games_wii"},
	    {"dir":"games_psp",
	     "name":"games_psp"}],
 "browse_min_glus":1,
 "email_addresses":[{"address":"mark@glyde.com",
		     "name":"Mark"}]
}
*)
