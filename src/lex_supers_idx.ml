open Util;;  open Printf;;  open ExtList

let dir_name = try Sys.argv.(1) with _ -> "."

let _ =
  let tbl = Int_ary_tbl.open_tbl dir_name "lex.supers" 
  and lxn = Lexicon.open_tbl dir_name in
    Int_ary_tbl.iteri
      (fun k vs ->
	 (* printf "%d : %s\n" k (Int_ary.to_json_ary vs); *)
	 (*
	   printf "%s : %s\n" (Lexicon.get lxn k)
	     (String.concat ", " (Int_ary.map (Lexicon.get lxn) (Int_ary.take 20 vs)))
	 *)
	 (*
	 match Int_ary.length vs with
	   | 0 -> ()
	   | x -> printf "%s : %d\n" (Lexicon.get lxn k) x
	 *)
	 (* printf "%s : %d\n" (Lexicon.get lxn k) (Int_ary.length vs) *)
	 printf "%d :: %s :: %d\n" k (Lexicon.get lxn k) (Int_ary.length vs)
      )
      tbl

