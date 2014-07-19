open Pcre;;  open Util

let make_re_from_strs (strs:string list) : regexp =
  regexp ("^(" ^ (String.concat "|" strs) ^ ")$")

let (charities_re, browse_re, games_re, ce_re, tablets_re, mp3s_re, phones_re, laptops_re, accessories_re) =
  let f = make_re_from_strs in
    (f (all_prefixes "charity"),
     f (all_prefixes "browse"),
     f ("game's" :: (all_prefixes "games")),
     f (("tablet's" :: (all_prefixes "tablets")) @ ("mp3's" :: (all_prefixes "mp3s")) @ ("phone's" :: (all_prefixes "phones")) @ ("laptop's" :: (all_prefixes "laptops"))),
     f ("tablet's" :: (all_prefixes "tablets")),
     f ("mp3's" :: (all_prefixes "mp3s")),
     f ("phone's" :: (all_prefixes "phones")),
     f ("laptop's" :: (all_prefixes "laptops")),
     f ("accessory's" :: (all_prefixes "accessories"))
    )

(* to do: move this into config files. *)
let rex_for_domain : string -> regexp = function
  | "charities" -> charities_re
  | "browse"    -> browse_re
  | "games" | "games_platforms" -> games_re
  | "ce"        -> ce_re
  | "tablets"   -> tablets_re
  | "mp3s"      -> mp3s_re
  | "phones"    -> phones_re
  | "laptops"   -> laptops_re
  | "accessories" -> accessories_re
  | x -> make_re_from_strs (all_prefixes x)

(* No longer in use. *)
let is_query_lexeme_short (domain_name:string) (lxm:string) : bool =
  let len = float_of_int |> String.length in
    len lxm /. len domain_name < 0.5

let from_str_query_tree (qt:Str_query_tree.t) (domain:Domain.t)
    : Id_query_tree.t option =
  let match_p = pmatch ~rex:(rex_for_domain (Domain.name domain)) in
    match Str_query_tree.sans_matches qt match_p with
	(* if no matches, OR if *every* node matches, then we have no 'sans' query. *)
      | ([], _)
      | (_, Str_query_tree.Empty_node) ->
	  None
      | (lxms, sans_qt) ->
	  let id_sans_qt = Id_query_tree.from_str_tree sans_qt (Domain.lexicon domain) in
	    Some id_sans_qt
