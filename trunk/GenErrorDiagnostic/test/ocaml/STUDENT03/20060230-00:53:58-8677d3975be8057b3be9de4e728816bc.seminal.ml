
exception Unimplemented

let char2str c = String.make 1 c

let reverse str = 
        let rec reverse_helper src dest = 
                if String.length src == String.length dest 
                then dest
                else reverse_helper src (src.[String.length dest] ^ dest) in
        reverse_helper str ""

let testreverse = print_string (reverse "hello world")

let fold_left f init str =
  raise Unimplemented

let fold_right f str init =
  raise Unimplemented

let map f str =
  raise Unimplemented


let uppercase s =
  raise Unimplemented

let lowercase s =
  raise Unimplemented

let titlecase s =
  raise Unimplemented
    
let histogram s =
  raise Unimplemented
(* 10,41-65 *)
