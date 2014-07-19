
let input_lines (file_name:string) : string Enum.t =
  Std.input_lines (open_in file_name)
      
let names_in_dir (dir_name:string) : string list =
  let dir = Unix.opendir dir_name in
  let rec loop acc = try loop (Unix.readdir dir :: acc) with End_of_file -> acc
  in List.rev (loop [])
