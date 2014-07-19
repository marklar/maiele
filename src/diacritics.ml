open Util;;  open ExtList

(*
  Internally, we're doing everything as Latin-1.
    http://en.wikipedia.org/wiki/ISO/IEC_8859-1
  The "diac_codes" here are both upper- and lowercase.
*)

let (diac_2_reg, reg_2_diac) =
  let reg_2_diac_codes_alist =
    [ ('a', 170 :: range 192 197 @ range 224 229)
    ; ('c', [162; 169; 199; 231])
    ; ('d', [208])
    ; ('e', range 200 203 @ range 232 235)
    ; ('i', range 204 207 @ range 236 239)
    ; ('n', [182; 209; 241])
    ; ('o', [176; 186; 216; 240; 248] @ range 210 214 @ range 242 246)
    ; ('p', [222; 254])
    ; ('r', [174])
    ; ('s', [167; 223])
    ; ('u', 181 :: range 217 220 @ range 249 252)
    ; ('x', [215])
    ; ('y', [165; 221; 253; 255])
    ]
  and d2r = Hashtbl.create 128
  and r2d = Hashtbl.create 16 in
  let add_codes_to_dict (reg, diac_codes) =
    let chars = List.map char_of_int diac_codes in
      List.iter (fun c -> Hashtbl.add d2r c reg) chars;
      List.iter (Hashtbl.add r2d reg) chars
  in 
    List.iter add_codes_to_dict reg_2_diac_codes_alist;
    (d2r, r2d)

(*-- public --*)

let for_reg (ch:char) : char list =
  Hashtbl.find_all reg_2_diac ch

let fold (s:string) : string =
  for i = 0 to (String.length s) - 1 do
    try s.[i] <- Hashtbl.find diac_2_reg s.[i]
    with Not_found -> ()
  done;
  s
