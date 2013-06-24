
exception Unimplemented

let char2str c = String.make 1 c

let reverse str =
  let rec reverse_helper src dest = 
    if String.length src == String.length dest then dest
    else reverse_helper src (char2str src.[String.length dest] ^ dest) in
  reverse_helper str ""

let testreverse = print_string (reverse "hello world")

let fold_left f init str =
  raise Unimplemented

let fold_right f str init =
  raise Unimplemented

let map f str =
  let rec map_helper f i str res =
    if String.length str == i then res
    else map_helper f i+1 str res::(f str.[i])
  in
  map_helper f 0 str []

let testmap = map (fun a -> a) "hello world"

let rec printlist l = 
  match l with 
      [] -> ()
    | x::rest -> (print_string (char2str x); printlist rest)
  

let printtestmap = printlist testmap
  
  (* ################### *)


let uppercase s =
  raise Unimplemented

let lowercase s =
  raise Unimplemented

let titlecase s =
  raise Unimplemented
    
let histogram s =
  raise Unimplemented
(* 23,9-25 *)
