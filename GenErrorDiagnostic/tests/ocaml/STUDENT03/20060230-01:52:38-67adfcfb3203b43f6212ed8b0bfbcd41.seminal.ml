
exception Unimplemented

let char2str c = String.make 1 c

let reverse str =
  let rec reverse_helper src dest = 
    if String.length src == String.length dest then dest
    else reverse_helper src (char2str src.[String.length dest] ^ dest) in
  reverse_helper str ""

(* ############################################ *)

let fold_left f init str =
  let rec fold_left_helper f cur str i =
    if String.length str == i then cur
    else fold_left_helper f (f cur str.[i]) str (i+1) in
  fold_left_helper f init str 0

let _ = print_int fold_left (fun count _ -> count+1) "hello world"
  
let fold_right f str init =
  raise Unimplemented

let map f str =
  let rec map_helper f i str res =
    if 0 == i then res
    else map_helper f (i-1) str ((f str.[i-1])::res)
  in
  map_helper f (String.length str) str []

(*
##
###############################
###############
##############
#########################################################################

########################################
######################################

######################################################

###############
##############
##############
##############
##############
##############
##############
##############
##############
##############
##############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############
###############

################################################

*)
    
let uppercase s =
  raise Unimplemented

let lowercase s =
  raise Unimplemented

let titlecase s =
  raise Unimplemented
    
let histogram s =
  raise Unimplemented
(* 20,8-17 *)
