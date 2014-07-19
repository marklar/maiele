open Printf

type t = { id         : int
	 ; type_name  : string
	 ; avail_glus : int
	 ; total_glus : int
   ; glu_ids    : Int_ary.t
	 }

let create (id:int) (type_name:string) (avail_glus:int) (total_glus:int) (glu_ids:Int_ary.t): t =
  { id         = id
  ; type_name  = type_name    (* e.g. 'category', 'star', 'cast', 'creator' *)
  ; avail_glus = avail_glus
  ; total_glus = total_glus
  ; glu_ids    = glu_ids
  }

let show t (format:Request.format) : string =
  let pairs = [ ("id",   string_of_int t.id)
	      ; ("type", sprintf "\"%s\"" t.type_name)
	      ]
  in Show.show_pairs format (Some "tag") pairs

let sum_glus (f:t -> int) ts : int = List.fold_left (fun m t -> m + (f t)) 0 ts
let sum_avail_glus : t list -> int = sum_glus (fun t -> t.avail_glus)
let sum_total_glus : t list -> int = sum_glus (fun t -> t.total_glus)
