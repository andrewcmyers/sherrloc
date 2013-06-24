
exception Unimplemented

let char2str c = String.make 1 c

let reverse str =
  let rec reverse_helper src dest = 
    if String.length src == String.length dest then dest
    else reverse_helper src (char2str src.[String.length dest] ^ dest) in
  reverse_helper str ""

(* ############################################ *)

let fold_left f init str =
  raise Unimplemented

let fold_right f str init =
  raise Unimplemented

let map f str =
  let rec map_helper f i str res =
    if 0 == i then res
    else map_helper f (i-1) str ((f str.[i-1])::res)
  in
  map_helper f (String.length str) str []

let rec printlist l = 
  match l with 
      [] -> ()
    | x::rest -> (print_string (char2str x); printlist rest)

let _ = printlist (map (fun a -> a) "hello world")

let charmap c =
  match c with
      a -> 1
    | b -> 2
    | c -> 3
    | d -> 4
    | e -> 5
    | f -> 6
    | g -> 7
    | h -> 8
    | i -> 9
    | j -> 10
    | k -> 11
    | l -> 12
    | m -> 13
    | n -> 14
    | o -> 15
    | p -> 16
    | q -> 17
    | r -> 18
    | s -> 19
    | t -> 20
    | u -> 21
    | v -> 22
    | w -> 23
    | x -> 24
    | y -> 25
    | z -> 26
    | _ -> 9999

let _ = printlist (map charmap "hello world")
    
let uppercase s =
  raise Unimplemented

let lowercase s =
  raise Unimplemented

let titlecase s =
  raise Unimplemented
    
let histogram s =
  raise Unimplemented
(* 64,18-45 *)
