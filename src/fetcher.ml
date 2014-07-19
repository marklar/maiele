(**
   For testing performance.
*)

open Http_client.Convenience
open Util
open Printf
open Pcre

let _ =
  let str = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let uri = sprintf "http://127.0.0.1:10401/maiele/results.js?query=%s" (replace ~pat:"\\s+" ~templ:"%20" str) in
    printf "%s\n" (time_it str (fun () -> http_get uri))
