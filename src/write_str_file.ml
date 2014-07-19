open Pcre;;  open Printf;;  open Util;;  open Mysql;;  open ExtList

let domain_name_and_limit_and_dbd () : (string * int option * Mysql.dbd) =
  let x = Sys.argv in
  let (domain_name, dbhost, dbport, dbname, dbuser) =
    try
      (x.(1), x.(2), int_of_string x.(3), x.(4), x.(5))
    with _ -> failwith
      (sprintf
	 "usage: %s <domain_name> <db_host> <db_port> <db_name> <db_user> [<db_pwd>] [<limit> (testing)]"
	 x.(0))
  and dbpwd =
    try Some x.(6)
    with _ -> None
  and limit =
    try Some (int_of_string x.(7))
    with _ -> None
  in
    
  (* cfg: begin *)
  let db_info:Mysql.db =
    { dbhost = Some dbhost  (* "dbhost" *)
    ; dbport = Some dbport  (* 3306 *)
    ; dbname = Some dbname  (* "peeko_mark" *)
    ; dbuser = Some dbuser  (* "mark" *)
    ; dbpwd  = dbpwd        (* None *)
    }
    (* cfg: end *)
  in
    (domain_name, limit, connect db_info)

let canon_and_searchable (title:string) (parenthetical:string)
    : string * string =
  let searchable = 
    (Util.compact_whitespace |> Entities.down)
      (title ^ (match parenthetical with "" -> "" | c -> sprintf " (%s)" c))
  in (Doc_lexer.canonicalize_str searchable, searchable)

let write_title (id:string) (rank:string) (title:string) (creator:string)
    : unit =
  if title = "" then
    ()
  else
    let (canon, searchable) = canon_and_searchable title creator in
      print_endline (String.concat "\t"
		       [ Doc_lexer.sort_len_str canon
		       ; canon
		       ; zero_pad 9 id
		       ; searchable
		       ; zero_pad 9 rank
		       ; title
		       ])

let write_one_tag (id:string) (rank:string) (name:string) (vertical:string)
    : unit =
  if name = "" then
    ()
  else
    let ui_vertical_name = match vertical with
      | "mp3s"   -> "iPods"
      | _        -> vertical
    in
    let (canon, searchable) = canon_and_searchable name ui_vertical_name in
      print_endline (String.concat "\t"
		       [ Doc_lexer.sort_len_str canon
		       ; canon
		       ; zero_pad 9 id
		       ; searchable
		       ; zero_pad 9 rank  (* necessary to pad? *)
		       ; ""
		       ])

let write_all (dbd:Mysql.dbd) (domain_name:string) (limit:int option) : unit =
  (* "series": deprecated code *)
  let write_tag = match domain_name with
    | "series" -> fun i r name v -> write_one_tag i r (Series.fix name v) v
    | _        -> write_one_tag  (* "tags" *)
  in
    iter (Mysql.exec dbd (Sql.for_domain domain_name limit))
      (function  (* string option array *)
	 | [|None|] -> ()
	 | [|Some id; Some rank; Some title; Some creator_or_vert|] ->
	     (* all domains but browse *)
	     write_title id rank title creator_or_vert
	 | [|Some id; Some rank; Some name; Some vertical; Some _|] ->
	     (* browse *)
	     write_tag id rank name vertical
	 | _ -> failwith "Wrong DB record format.")
       
let _ =
  if pmatch Sys.argv.(0) ~pat:"write_str_file$" then
    let (domain_name, limit, dbd) = domain_name_and_limit_and_dbd () in
      match domain_name with
	| "charities"
	| "games" | "ce" | "tablets" | "mp3s" | "phones" |"laptops" | "accessories"
	| "games_with_platforms"
	| "games_psp" | "games_ps2" | "games_ps3"
	| "games_xbox" | "games_xbox360"
	| "games_ds" | "games_3ds" | "games_gamecube" | "games_wii"
	| "people" | "actors" | "authors" | "musicians" | "vid_people"
	| "categories" | "languages" | "mus_labels" | "series" | "platforms"
	| "upcs" ->
	    write_all dbd domain_name limit
	| "browse" ->
	    List.iter
	      (fun dn -> write_all dbd dn limit)
	      ["tags"; "series"]
	| _ ->
	    failwith (sprintf "unsupported domain name: %s" domain_name)
