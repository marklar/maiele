(*
For idx / product codes.
Allows searching by UPC or EAN.
*)

open Util;; open Printf

type t =
	{ codes : Lexicon.t
		; code_2_glu : Int_ary.t      (* code _ID_ *)
		; glu_2_codes : Int_ary_idx.t
	}

let open_tbl (dir: string) : t =
	{ codes = Lexicon.open_tbl dir "code.lex"
		; code_2_glu = Int_ary.from_file dir "code:glu"
		; glu_2_codes = Int_ary_idx.open_tbl dir "glu:codes"
	}

let close t : unit =
	Lexicon.close t.codes;
	Int_ary.close t.code_2_glu;
	Int_ary_idx.close t.glu_2_codes

let code_from_id t (code_id: int) : string =
	Lexicon.get_exn t.codes code_id

let glu_for_code_id t (code_id: int) : int =
	Int_ary.get t.code_2_glu (code_id - 1)

let codes_for_glu t (glu_id: int) : string list =
	let code_ids = Int_ary_idx.get t.glu_2_codes glu_id in
		Int_ary.map
			(fun i -> Lexicon.get_exn t.codes i)
			code_ids

type code_w_glu = string * int

let code_and_glu t (code_id: int) : code_w_glu =
	(code_from_id t code_id, glu_for_code_id t code_id)

let glu_for_code t (code: string) : int option =
	match Lexicon.complete_match_id t.codes code with
	| None ->
			None
	| Some code_id ->
			Some (glu_for_code_id t code_id)

let first_n_code_glu_pairs t (n: int) (s: string) : code_w_glu list =
	let code_ids = Lexicon.n_matching_ids t.codes n s in
		List.map (code_and_glu t) code_ids

(*
iterator.
holds its state inside closure: next_code_id.
*)
let next_glu_id_fun t (s: string) : unit -> int option =
	let next_code_id = Lexicon.next_matching_id_fun t.codes s in
		(fun () ->
					match next_code_id () with
					| None -> None
					| Some code_id -> Some (glu_for_code_id t code_id)
		)
