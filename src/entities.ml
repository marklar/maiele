open Util;;  open Pcre;;  open Printf

let num_entity_digits_re = regexp "&#(\\d+);"
let numeric_entity_num (s:string) : int =
  int_of_string (Array.get (extract s ~rex:num_entity_digits_re ~full_match:false) 0)

(* Question: what to do about numbers > 255? *)
let first_num_entity_re = regexp "^(.*?)&#\\d+;"
let rec down_digits (s:string) : string =
  try
    let templ = sprintf "$1%c" (Char.chr (numeric_entity_num s)) in
      down_digits (replace s ~rex:first_num_entity_re ~templ)
  with _ -> s

(* be sure to do this AFTER downing digits *)
let amp_re = regexp "&amp;"
let amp_tm = subst "&"
let down_amps (s:string) : string =
  replace s ~rex:amp_re ~itempl:amp_tm

let mdash_re = regexp "&mdash;"
let mdash_tm = subst "-"
let down_mdashes (s:string) : string =
  replace s ~rex:mdash_re ~itempl:mdash_tm

let quot_re = regexp "&quot;"
let quote_tm = subst "\""
let down_quotes (s:string) : string =
  replace s ~rex:quot_re ~itempl:quote_tm

let wide_ellipses_re = regexp "\\. \\. \\."
let reg_ellipses_tm = subst "..."
let tighten_ellipses (s:string) : string =
  replace s ~rex:wide_ellipses_re ~itempl:reg_ellipses_tm

let hellip_re = regexp "&hellip;"
let hellip_tm = subst "..."
let down_hellips (s:string) : string =
  tighten_ellipses (replace s ~rex:hellip_re ~itempl:hellip_tm)

(*-- public --*)

let down : string -> string =
  down_amps |> down_mdashes |> down_hellips |>
      down_quotes |> down_digits
