
let file_descr (dir_name:string) (file_name:string) (shared:bool) : Unix.file_descr =
  let (open_flags, file_perm) = match shared with
    | false -> ([Unix.O_RDONLY], 0o440)     (* -r--r----- *)
    | true  -> ([Unix.O_RDWR],   0o640)     (* -rw-r----- *)
  in Unix.openfile (Filename.concat dir_name file_name) open_flags file_perm
