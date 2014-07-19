open Util;;  open Printf

(*
  Specifically for 'browse' index.
  For determining whether to show result.

  We keep track of how many available Glus are associated with a tag.
  (For a Glu to be "available" means for there to be in-stock Listings.)

  Each tag corresponds to an actual DB tag.
  Each 'browse' result may correspond to >1 tag.
  To determine whether to include a Result in our 'browse' search response,
  we need to know which tags it corresponds to,
  and the total number of available Glus for all those tags.

  This info is sent with each Result:
    {
      "tags": [{"id":1000745,"type":"creator"}],
      "avail_glus": 1,
      "total_glus": 8
    }

  The tag_type info (e.g. 'creator') is used by the client to
  update the genre selector text on the page.

  The tag ID(s) are used by the client to make a query to RGS.

  I don't believe the 'avail_glus' or 'total_glus' info is currently
  used by the browser.
*)

type t =
    { type_ids   : Int_ary.t
    ; avail_glus : Int_ary.t   (* counts *)
    ; total_glus : Int_ary.t
    ; glu_ids    : Int_ary_tbl.t
    ; len        : int
    }

(* 'Set-once'.  Requires fetching from DB. *)
let type_id_2_name_ref = ref None

(*
  Query against TAGS database: (id, name) for tag TYPEs.
  May raise (i.e. doesn't catch if DB connection fails).
*)
let get_type_id_2_name () : (int, string) Hashtbl.t =
  match !type_id_2_name_ref with
    | Some hash -> hash
    | None ->
	let hash = Hashtbl.create 20
	and db_res = Mysql.exec (Dbd.get_tag_dbd())  (* may raise *)
	  "SELECT id,name FROM glu_tag_types ORDER BY id"
	in
	  (* update 'hash' *)
	let rec loop = function
	  | None -> ()
	  | Some [|Some type_id; Some type_name|] ->
	      Hashtbl.add hash (Mysql.int2ml type_id) type_name;
	      loop (Mysql.fetch db_res)
	  | _ -> raise Not_found
	in
	  loop (Mysql.fetch db_res);
	  hash

(*
  Tries to use hash from DB data.
  If getting a DB connection fails, we're S.O.L.
*)
let type_name_from_type_id (id:int) : string =
  Hashtbl.find (get_type_id_2_name()) id 

let type_id t (id:int) : int =
  Int_ary.get t.type_ids (id-1)

let type_name t (id:int) : string =
  type_name_from_type_id (type_id t id)

let avail_glus t (id:int) : int =
  Int_ary.get t.avail_glus (id-1)

let total_glus t (id:int) : int =
  Int_ary.get t.total_glus (id-1)

let glu_ids t (id:int) : Int_ary.t =
  Int_ary_tbl.get_exn t.glu_ids id

(* public *)

let open_tbl (dir:string) : t =
  let type_ids = Int_ary.from_file dir "tag.type_ids.data" in
    { type_ids   = type_ids
    ; avail_glus = Int_ary.from_file ~shared:true dir "tag.avail_glus.data"
    ; total_glus = Int_ary.from_file dir "tag.total_glus.data"
    ; glu_ids    = Int_ary_tbl.open_tbl dir "tag.glu_ids"
    ; len        = Int_ary.length type_ids
    }

let close t : unit =
  List.iter Int_ary.close
    [ t.type_ids
    ; t.avail_glus
    ; t.total_glus
    ];
  Int_ary_tbl.close t.glu_ids

let fetch_exn t (id:int) : Tag.t =
  if id <-> (1, t.len) then
    Tag.create id (type_name t id) (avail_glus t id) (total_glus t id) (glu_ids t id)
  else
    invalid_arg "Tag_tbl.fetch: id out of range."

let fetch_all_exn t (ids:int list) : Tag.t list =
	List.map (fetch_exn t) ids 

let set_avail_glus t (id: int) (num: int) : unit =
	Int_ary.set t.avail_glus (id-1) num
