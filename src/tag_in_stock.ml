open Pcre;;

let int_of_bool (b:bool) : int =
  if b then
    1
  else
    0

let recalc_avail_glus (glu_tbl : Glu_tbl.t) (tag_tbl : Tag_tbl.t) (tag_id : int) : unit =
  let sum_avail_glus = 
	  let glu_ids = Tag_tbl.glu_ids tag_tbl tag_id
	  and incr_if_in_stock memo glu_id =
	    memo + int_of_bool (Glu_tbl.in_stock glu_tbl glu_id)
    in
	    Int_ary.fold_left incr_if_in_stock 0 glu_ids
  in
    Tag_tbl.set_avail_glus tag_tbl tag_id sum_avail_glus

let set_in_stock (glu_id: int) (in_stock: bool) : unit =
	try
		let result_tbl =
			let dom = Searcher.domain_of_name "browse" in
				Domain.result_tbl dom
		in
		  match Result_tbl.glu_tbl result_tbl with
				| None -> raise Not_found
				| Some glu_tbl ->
					Glu_tbl.set_in_stock glu_tbl glu_id in_stock;
					let tag_ids = Glu_tbl.tag_ids glu_tbl glu_id in
					  match Result_tbl.tag_tbl result_tbl with
							| None -> raise Not_found
							| Some tag_tbl ->
								Int_ary.iter (recalc_avail_glus glu_tbl tag_tbl) tag_ids
	with Not_found ->
		failwith "Tag_in_stock.set_in_stock: no browse index or browse index is broken"