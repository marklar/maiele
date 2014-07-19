
(*
  For purpose of making it easier (terser) to pass around lots of arguments.
    Controller => Search_fun => Searcher => Domain

  'in_stock_p' has 2 different meanings, depending on the search domain.
     - charities: FILTER results by whether the charity has any inventory.
     - verticals: decorate results w/ whether in_stock or not.
*)


(*
  ADD?:
   - query_str
   - domain_names
*)

type t =
    { show_size  : int
    ; offset     : int
    ; limit      : int
    ; in_stock_p : bool
    ; sellable_p : bool
    ; bop_p      : bool
    ; pc_p       : bool
    ; format     : Request.format
    }

(* needed? *)
let with_limit t n : t =
  { t with limit = n }

(* modify this to make some args optional (with defaults) *)
let make (show_size:int)
    (offset:int) (limit:int)
    (in_stock_p:bool) (sellable_p:bool) (bop_p:bool) (pc_p:bool)
    (format:Request.format)
    : t =
  (* this boilerplate can be scrapped with OCaml 3.12 *)
  { show_size    = show_size
  ; offset       = offset
  ; limit        = limit
  ; in_stock_p   = in_stock_p
  ; sellable_p   = sellable_p
  ; bop_p        = bop_p
  ; pc_p         = pc_p
  ; format       = format
  }

let default =
  { show_size  = 5
  ; offset     = 0
  ; limit      = 20
  ; in_stock_p = false
  ; sellable_p = false
  ; bop_p      = false
  ; pc_p       = false
  ; format     = `Json
  }

let show_size    t = t.show_size
let offset       t = t.offset
let limit        t = t.limit
let in_stock_p   t = t.in_stock_p
let sellable_p   t = t.sellable_p
let bop_p        t = t.bop_p
let pc_p         t = t.pc_p
let format       t = t.format

let fetch_size t : int =
  t.offset + t.limit
