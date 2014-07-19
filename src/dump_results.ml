open Util;;  open Printf

let show_result (r:Result.t) : unit =
  if not (Result.is_faux r) then
    printf "%s\t%s\n"
    (Result.text r)
    (Int_ary.to_json_ary (Result.target_ids r))
  else ()

let usages (dir_name:string) : unit =
  Result_tbl.iter show_result $ Result_tbl.open_tbl dir_name

let _ =
  let dir_name =
    try Sys.argv.(1)
    with _ -> failwith "usage: dump_usages <dir_name>"
  in usages dir_name
