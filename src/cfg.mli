(** exported for use in Dbd *)
val json_tbl : (string, Json_type.t) Hashtbl.t

(** indices *)
type index
val idx_name : index -> string
val idx_dir  : index -> string
val idx_root_dir : string
val indices : index list

(** Jo_lsd *)
val jo_parallel_p : bool
val jo_timeout_secs : float
val email_addresses : (string * string) list

(** domains *)
val is_in_stock_domain : string -> bool
val all_domain_names : string list
val all_buy_domain_names : string list
val all_sell_domain_names : string list
val glyde_storefront_name : string

(** browse (lsyncd) *)
val browse_min_glus : int
val solr_host : string
val solr_port : int
val lsync_frequency_secs : float
