module type INDEX_DATA_TYPE = sig
  type t
  val push : 'a IO.output -> t -> unit
end

module Make =
  functor (Data:INDEX_DATA_TYPE) -> struct

    type data_type = Data.t

    type t =
	{ path      : string           (* for debug *)
	; out_chan  : out_channel      (* for open, close, and pos *)
	; io_output : unit IO.output   (* for push.  IO: from extLib *)
	}

    let create (dir:string) (filename:string) : t =
      let path = Filename.concat dir filename in
      let out_chan = open_out_bin path in
	{ path      = path
	; out_chan  = out_chan
	; io_output = IO.output_channel out_chan
	}

    let push t (data:data_type) : unit =
      Data.push t.io_output data

    let pos t : int =
      pos_out t.out_chan
	
    let close t : unit =
      flush t.out_chan;
      close_out t.out_chan

    let write_all (dir:string) (filename:string) (data:data_type) : unit =
      let t = create dir filename in
	push t data;
	close t
  end

(*-- particular implementations --*)

module StringData = struct
  type t = string
  let push (io_output:'a IO.output) t : unit = IO.nwrite io_output t
end
module String = Make(StringData)

(*
  Bool and Int make no sense as part of a "tbl".
  Both data types are fixed-width already.
*)
module BoolData = struct
  type t = bool
  let push (io_output:'a IO.output) t : unit =
    IO.write_byte io_output (if t then 1 else 0)
end
module Bool = Make(BoolData)


let push_int (io_output:'a IO.output) (i:int) : unit =
  IO.write_real_i32 io_output (Int32.of_int i)

(* see note for BoolData *)
module IntData = struct
  type t = int
  let push = push_int
end
module Int = Make(IntData)

module IntListData = struct
  type t = int list
  let push (io_output:'a IO.output) t : unit =
    List.iter (push_int io_output) t
end
module IntList = Make(IntListData)

module IntArrayData = struct
  type t = int array
  let push (io_output:'a IO.output) t : unit =
    Array.iter (push_int io_output) t
end
module IntArray = Make(IntArrayData)

module IntAryData = struct
  type t = Int_ary.t
  let push (io_output:'a IO.output) t : unit =
    Int_ary.iter (push_int io_output) t
end
module IntAry = Make(IntAryData)
