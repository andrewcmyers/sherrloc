
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

(* 

#######################################################################

#######################################################################################

 *)
  
let fold_right f init str =
  let rec fold_right_helper f cur str i =
    if 0 == i then cur
    else fold_right_helper f (f cur str.[i-1]) str (i-1) in
  fold_right_helper f init str (String.length str)

(* 

#######################################################################

########################################################################################
*)
 
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

let map_string f str = 
  let rec map_string_helper f i str res =
    if 0 == i then res
    else map_string_helper f (i-1) str ((f str.[i-1]) ^ res)
  in
  map_string_helper f (String.length str) str ""    

let uppercase s = map_string Char.uppercase s

let _ = uppercase "hello world"

let lowercase s =
  raise Unimplemented

let titlecase s =
  raise Unimplemented
    
let histogram s =
  raise Unimplemented
(* 101,29-43 *)
