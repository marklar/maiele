(* use this *before* create_faux *)
open Printf

let _ =
  let dir_name = try Sys.argv.(1) with _ -> failwith (sprintf "usage: %s <dir_name>" Sys.argv.(0)) in

  (* Open 'pop' partition.  We'll modify in situ. *)
  let pops = Int_ary.from_file ~shared:true dir_name "/res.pop.data" in

  (* Create Array to hold tuples: (pop, id) *)
  let pop_id_pairs =
    Array.init (Int_ary.length pops) (fun i -> (Int_ary.get pops i, i))
  in
    (* Sort in situ. *)
    Array.fast_sort compare pop_id_pairs;
    (* In 'pop' partition, update pop's value for id (snd pair). *)
    Array.iteri (fun i pair -> Int_ary.set pops (snd pair) (i+1)) pop_id_pairs;
    Int_ary.close pops
