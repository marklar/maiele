(*
  A "_tbl" contains two files:
    - offsets : maps ID -> data offset
    - data    : variable-length data

  This functor parameterizes over the type of Idx_file
  (itself a functor) to serve as the "data" file maker.
  (See idx_file.ml for particular implementations
  (instances) of Idx_file.)
  
  So, "Make" an instance of this functor to create a "table maker"
  for the particular type of data you'd like to store.
  e.g.
      module Int_list_tbl_maker = Tbl_mkr.Make(Idx_file.IntListData)
*)

module Make =
  functor (Data:Idx_file.INDEX_DATA_TYPE) -> struct

    type data_type = Data.t

    (* index files *)
    module Offs_IF = Idx_file.Int
    module Data_IF = Idx_file.Make(Data)

    type t =
	{ offs : Offs_IF.t
	; data : Data_IF.t
	}

    let create (dir:string) (root:string) : t =
      let sfx = (^) root in
	{ offs = Offs_IF.create dir (sfx ".offs")
	; data = Data_IF.create dir (sfx ".data")
	}

    let push t (v:data_type) : unit =
      Offs_IF.push t.offs (Data_IF.pos t.data);
      Data_IF.push t.data v

    let close t : unit =
      Offs_IF.close t.offs;
      Data_IF.close t.data

    let with_tbl (dir:string) (root:string) (f:t -> 'a) : 'a =
      let t = create dir root in
      let res = f t in
	close t; res

    let write_all (dir:string) (root:string) (vs:data_type list) : unit =
      let t = create dir root in
	List.iter (push t) vs;
	close t
  end
