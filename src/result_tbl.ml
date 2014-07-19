open Util;; open ExtList

type t =
    { pop_ary          : Int_ary.t
    ; text_tbl         : Str_tbl.t
    ; target_ids_tbl   : Int_ary_tbl.t
    ; pop_2_id_ary     : Int_ary.t
    ; target_idx       : Int_ary_idx.t
    ; len              : int
    ; is_faux_ary      : Bool_ary.t option (* for games_* *)
    ; tag_tbl          : Tag_tbl.t option  (* for browse *)
		; glu_tbl          : Glu_tbl.t option  (* for browse *)
    ; store_ids_tbl    : Int_ary.t option  (* for charities *)
    ; product_code_tbl : Product_code_tbl.t option
    }

let open_tbl (dir:string) : t =
  let opt f =  (* creates an option *)
    try Some (f ())
    with _ -> None
  in
  let p = Int_ary.from_file dir "res.pop.data" in
    { pop_ary        = p
    ; len            = Int_ary.length p
    ; text_tbl       = Str_tbl.open_tbl     dir "res.text"
    ; target_ids_tbl = Int_ary_tbl.open_tbl dir "res.target_ids"
    ; pop_2_id_ary   = Int_ary.from_file    dir "res.pop.idx.data"
    ; target_idx     = Int_ary_idx.open_tbl dir "res.target_ids.idx"
    ; is_faux_ary   =
	opt (fun () -> Bool_ary.from_file dir "res.is_faux.data")
    ; tag_tbl       =
	opt (fun () -> Tag_tbl.open_tbl   dir)
		; glu_tbl       =
	opt (fun () -> Glu_tbl.open_tbl   dir)
    ; store_ids_tbl =
	opt (fun () -> Int_ary.from_file  dir "store.data")
    ; product_code_tbl =
	opt (fun () -> Product_code_tbl.open_tbl dir)
    }

let tag_tbl t : Tag_tbl.t option =
  t.tag_tbl

let glu_tbl t : Glu_tbl.t option =
  t.glu_tbl

let product_code_tbl t : Product_code_tbl.t option =
  t.product_code_tbl

let close t : unit =
  List.iter Int_ary.close [t.pop_ary; t.pop_2_id_ary];
  Str_tbl.close      t.text_tbl;
  Int_ary_tbl.close  t.target_ids_tbl;
  Int_ary_idx.close   t.target_idx;
  let maybe f = function  (* execs fun or not, based on option arg *)
    | Some x -> f x
    | None -> ()
  in
    maybe Bool_ary.close         t.is_faux_ary;
    maybe Tag_tbl.close          t.tag_tbl;
		maybe Glu_tbl.close          t.glu_tbl;
    maybe Int_ary.close          t.store_ids_tbl;
    maybe Product_code_tbl.close t.product_code_tbl

let length t : int = t.len

(* inside pop(), is_faux(), id_from_pop()...
   ARRAYS, so expect INDEX. *)

let pop t (id:int) : int =
  Int_ary.get t.pop_ary (id-1)

let is_faux t (id:int) : bool =
  match t.is_faux_ary with
    | None -> false
    | Some ary -> Bool_ary.get ary (id-1)

let id_from_pop t (pop:int) : int =
  Int_ary.get t.pop_2_id_ary (pop-1)

(* inside text(), target_ids()...
   TABLES, so expect record ID *)

let text t (id:int) : string =
  Str_tbl.get_exn t.text_tbl id

let target_ids t (id:int) : Int_ary.t =
  Int_ary_tbl.get_exn t.target_ids_tbl id

let ids_from_target_id t (target_id:int) : Int_ary.t =
  Int_ary_idx.get t.target_idx target_id

let tags t (id:int) : Tag.t list option =
  match t.tag_tbl with
    | None -> None
    | Some tbl ->
	Some ( Int_ary.map
		 (fun tag_id -> Tag_tbl.fetch_exn tbl tag_id)
		 (target_ids t id) )

(*
  charity IDs are user IDs.
  Get charity (i.e. user) IDs for this charity name,
  and then use that to get the storefront IDs.
*)
let store_ids t (id:int) : int list option =
  match t.store_ids_tbl with
    | None -> None
    | Some tbl ->
	Some ( Int_ary.map
		 (fun charity_id -> Int_ary.get tbl (charity_id-1))
		 (target_ids t id) )

let product_codes t (id:int) : string list =
  match t.product_code_tbl with
    | None -> []
    | Some tbl ->
	List.sort ~cmp:compare  
	  ( List.concat
	      ( Int_ary.map (Product_code_tbl.codes_for_glu tbl)
		  (target_ids t id)
	      )
	  )

let fetch_exn t (id:int) : Result.t =
  if id <-> (1, t.len) then
    Result.create id (pop t id) (text t id)
      (target_ids t id) (is_faux t id)
      (product_codes t id)
      (tags t id) (store_ids t id)
  else
    invalid_arg "Result_tbl.fetch: index out of bounds"

let iter (f:Result.t -> 'a) t : unit =
  for i = 1 to t.len do
    f (fetch_exn t i)
  done
