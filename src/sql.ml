open Printf;;  open Pcre;;  open Util

(*
  We fetch data from the Glu database tables
  in order to create index results tables.
  Each search domain has a different SQL query
  for fetching its data.
  This module is for creating those SQL queries.
*)

(*
  Do NOT use max_int for these.
  That number is far too large,
  making the field in the "string file" far too wide.
*)
let max_possible_assoc_count = 10_000_000
let max_amazon_rank = 9_999_999

(*
  glu_id, rank, title, creator|""

  For games, ignore glu_ufa_writable.is_listable, as we wish to include unreleased items
*)
let games_sql (limit_str:string) : string =
  sprintf
    "SELECT g.id, IFNULL(MIN(gr.amazon_rank), %d), g.title, ''
       FROM glus AS g, glu_releases as gr, glu_ufa_writeable as guw
      WHERE g.vertical = 'games'
        AND gr.glu_id  = g.id
        AND guw.glu_id = g.id
        AND !g.is_deprecated AND g.catalog_is_listable AND guw.is_listable
      GROUP BY g.id
         %s"
     max_amazon_rank limit_str

(*
  glu_id, rank, title, ""
*)
let tablets_mp3s_phones_accessories_laptops_sql (domain_name:string) (limit_str:string) : string =
  sprintf
     "SELECT g.id, IFNULL(MIN(gr.amazon_rank), %d), g.title, ''
        FROM glus AS g, glu_releases as gr, glu_ufa_writeable as guw
       WHERE g.vertical = '%s'
         AND gr.glu_id  = g.id
         AND guw.glu_id = g.id
         AND !g.is_deprecated AND g.catalog_is_listable AND guw.is_listable
    GROUP BY g.id
          %s"
     max_amazon_rank domain_name limit_str

(*
  glu_id, rank, title, ""
*)
let ce_sql (limit_str:string) : string =
  sprintf
    "SELECT g.id, IFNULL(MIN(gr.amazon_rank), IFNULL(gr.glyde_rank, %d)), g.title, ''
       FROM glus AS g, glu_releases as gr, glu_ufa_writeable as guw
      WHERE g.vertical IN ('tablets', 'mp3s', 'phones')
        AND gr.glu_id  = g.id
        AND guw.glu_id = g.id
        AND !g.is_deprecated AND g.catalog_is_listable AND guw.is_listable
   GROUP BY g.id
         %s"
    max_amazon_rank limit_str

let glyde_platform_name : string -> string = function
    (* Microsoft *)
  | "games_xbox"     -> "Xbox"
  | "games_xbox360"  -> "Xbox 360"
      (* Sony *)
  | "games_psp"      -> "Sony PSP"
  | "games_ps2"      -> "PlayStation 2"
  | "games_ps3"      -> "PlayStation 3"
      (* Nintendo *)
  | "games_ds"       -> "Nintendo DS"  (* for DSi, too *)
  | "games_3ds"      -> "Nintendo 3DS"
  | "games_gamecube" -> "GameCube"
  | "games_wii"      -> "Wii"
  | dn -> failwith (sprintf "No Glyde platform name to match domain name (%s)." dn)

(*
  glu_id, rank, title, ""

  For games, ignore glu_ufa_writable.is_listable, as we wish to include unreleased items
*)
let games_one_platform_sql (domain_name:string) (limit_str:string) : string =
  sprintf
    "SELECT g.id, IFNULL(MIN(gr.amazon_rank), %d), g.title, ''
       FROM glus AS g, glu_game_data AS gd, glu_releases AS gr, glu_ufa_writeable as guw
      WHERE g.vertical = 'games'
        AND gd.platform = '%s'
        AND gr.glu_id  = g.id
        AND gd.glu_id  = g.id
        AND guw.glu_id = g.id
        AND !g.is_deprecated AND g.catalog_is_listable
   GROUP BY g.id
         %s"
    max_amazon_rank (glyde_platform_name domain_name) limit_str

(*
  glu_id, rank, title, platform

  For games, ignore glu_ufa_writable.is_listable, as we wish to include unreleased items
*)
let games_with_platforms_sql (limit_str:string) : string =
  sprintf
    "SELECT g.id, IFNULL(MIN(gr.amazon_rank), %d), g.title, gd.platform
       FROM glus AS g, glu_game_data AS gd, glu_releases AS gr, glu_ufa_writeable as guw
      WHERE g.vertical = 'games'
        AND gr.glu_id  = g.id
        AND gd.glu_id  = g.id
        AND guw.glu_id = g.id
        AND !g.is_deprecated AND g.catalog_is_listable
   GROUP BY g.id
         %s"
    max_amazon_rank limit_str

(*
  glu_id, rank, upc_or_ean, ""
  For live-searching EANs/UPCs.
*)
let upcs_sql (limit_str:string) : string =
  let s (upc_or_ean:string) : string = 
    sprintf
      "SELECT glu_id, IFNULL(amazon_rank, %d), %s, '' FROM glu_releases %s"
      max_amazon_rank upc_or_ean limit_str
  in (s "upc") (*  ^ "; " ^ (s "ean") *)

(*
  glu_tag_id, rank (sorta), tag_name, vertical, glu_tag_type_id (gets ignored)
*)
let browse_sql ?(condition="") (glu_tag_type_names:string list) (limit_str:string) : string =
  sprintf
    "SELECT gt.id, (%d - gt.assoc_count), gt.name, gt.vertical, gtt.id
       FROM glu_tags gt, glu_tag_types gtt
      WHERE gt.blacklisted = 0
        AND gt.assoc_count is not NULL
        AND gt.glu_tag_type_id = gtt.id
        AND gtt.name in (%s)
        AND gt.vertical != 'laptops'
         %s
         %s"
    max_possible_assoc_count
    (String.concat "," (List.map (sprintf "'%s'") glu_tag_type_names))
    condition
    limit_str

(*
  Grab name (and tagline?) from table charities (count: 5433).
  And:  name from charity_categories (count: 9.   e.g.: "Education", "Environment").
  Also: name from charity_causes     (count: 34.  e.g.: "Other Education Programs and Services").

  charity_id, revenue_rank, name, ""
 *)
let max_total_revenue_in_k = 20_000_000
let charities_sql (limit_str:string) : string =
  sprintf
    "SELECT id, (%d - IFNULL(total_revenue_in_k, 0)), name, ''
       FROM charities
        %s"
    max_total_revenue_in_k
    limit_str

(*
  NOT: languages, rating, company, relation, edition, product.  And as of #9100, not star or cast either.
  'series' is fetched separately.
*)
let tag_type_names = [ "category"; "keyword"; "creator"; "classifications"; "platform"; "item_type"; "family"; "other" ]

let for_domain (domain_name:string) (limit:int option) : string =
  let partial_application = match domain_name with
	(* non-standard games indices *)
    | "games_with_platforms" -> games_with_platforms_sql
    | "games_psp" | "games_ps2" | "games_ps3"
    | "games_wii" | "games_ds" | "games_3ds" | "games_gamecube"
    | "games_xbox" | "games_xbox360" -> games_one_platform_sql domain_name
	(* other *)
    | "charities" -> charities_sql
    | "games" -> games_sql
    | "tablets" | "mp3s" | "phones" | "accessories" | "laptops" -> tablets_mp3s_phones_accessories_laptops_sql domain_name
    | "ce" -> ce_sql
    | "tags"   -> browse_sql  tag_type_names
    | "series" -> browse_sql  ["series"]      (* all games [950] *)
    | _ -> failwith (sprintf "unsupported domain name: %s" domain_name)
  in partial_application ( match limit with
			     | None -> ""
			     | Some x -> sprintf "LIMIT %d" x )
