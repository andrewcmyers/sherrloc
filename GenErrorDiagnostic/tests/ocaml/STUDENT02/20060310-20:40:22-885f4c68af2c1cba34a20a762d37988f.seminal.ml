open Pprintdata

(**************** Pretty printer library *****************)

(* ####################################################################
###################################################################
###############################################################
#####################################################

###################################################################
############################################################################
##################################################################
#########################
#########################################################
#######################################################################
########################################################################
############################################# *)

let rec printChars st len =
  if (st > 0) then 
  (
    printChars (st ^ " ")  (len-1)
  ) else (
    st
  )
  
let rec sdocToString doc =
  match doc with
  | SNil        -> ""
  | SText(s,d)  -> s ^ sdocToString(d)
  | SLine(i,d)  -> 
	  let chStr = printChars("" 4) in
	  ""
 

(* #############################################################
#########################################
#############################################################################
########################################## *)
let rec fits w imdList = 
  Pprintdata.fits w imdList


(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
let rec format w k imdList = 
(*
####################
######################################
###############
#####
###################
##########################
#################
#############################################################
############################
###########################
############################
###########################
############################
#################
###################################################################
###########################
############################
##########################
################################################
#######################
###################
#######################################################
##################################################
#########################################################
####################################
##########################
##########################################
################################
############################
############
##############################
#####
#####
*)

  Pprintdata.format w k imdList


(* #################################################################
##################################################### *)
let pretty w doc =
  let sDc = format w 0 ([0, Flat, doc]) in 
(* #
################
#################################################
#################################################
#########################################
*)
  print_string "\n\n\n";
  sdocToString sDc
(*
#########################
*)

(* ################################# *)
let pretty_print w doc =
  print_endline (pretty w doc)

(*************** pretty-printer clients ******************)

open Ast

let rec expToDoc exp =
  match exp with
    Int i -> text (string_of_int i)
  | Var v -> text v
  | Plus(e1, e2) -> 
      group [nest 2 [text "(" ; br ; expToDoc e1 ;
                     br ; text "+" ; 
                     br ; expToDoc e2 ; br ; text ")"] ]
  | Times(e1, e2) -> 
      group [nest 2 [text "(" ; br ; expToDoc e1 ;
                     br ; text "*" ; 
                     br ; expToDoc e2 ; br ; text ")"] ]

let rec stmtToDocTest stmt = 
  Pprintdata.stmtToDoc stmt 

let rec stmtToDoc stmt = 
  match stmt with
    Skip -> empty
  | Assign(str, e) -> 
      group [nest 2 [text str ; br ; text ":=" ; br ; expToDoc e] ]
  | Seq(s1, s2) -> 
      smartcons (group [stmtToDoc s1 ; text ";"]) (conslist[br ; stmtToDoc s2])
  | If(e, s1, s2) ->
      group [ group [text "if" ; br ; expToDoc e] ; br ;
					nest 2 [text "(" ; br ; group[stmtToDoc s1] ] ; 
					br ; text ")" ; br ; 
					nest 2 [text "(" ; br ; group[stmtToDoc s2] ] ; 
					br ; text ")"]
  | While(e, s) -> 
      group [ group [text "while" ; br ; expToDoc e] ; br ; 
					nest 2 [text "(" ; br ; group [stmtToDoc s] ] ; 
					br ; text ")"]

type xmlAst = Element of 
  string                      (* ############### *)
    * (string * string) list  (* ######################################### *)
    * xmlAst list             (* ######################### *)

let xmlToDoc xml = 
  raise Unimplemented

(******************** pretty-printer test *************)
let get_prog_f filename =
  let ch = open_in filename in
  let res = Parse.program Lex.lexer (Lexing.from_channel ch) in
  close_in ch;
  res
let get_prog_s str = Parse.program Lex.lexer (Lexing.from_string str)

let test_prog filename =
  (* ######################################## *)
  let stmt = get_prog_f filename in
  let dc  = stmtToDoc stmt in
  let str = pretty 30 dc in
    print_string str;
    print_string "\n\n\n";
  (* ########################################## *)
  let stmt2 = get_prog_s str in
  let dc2  = stmtToDoc stmt2 in
  let str2 = pretty 30 dc2 in
    print_string str2;
  (* ################################################## *)
  if str = str2 then true else false 
    
(********************* Other ***********************)

(* #######################################################################
#################################### *)
let linewidth n = 
  let strn = string_of_int(n) in
  let charsstrn = String.length strn in
  let n = n-2-charsstrn in
  let (left, right) = if (n mod 2 = 0) 
                      then (n/2, n/2)
                      else ((n+1)/2,(n-1)/2) in
  "<" ^ (String.make left '-') ^ strn ^ (String.make right '-') ^ ">\n"






let test_prog_2 filename =
  (* ######################################## *)
  let stmt = get_prog_f filename in
  let dc  = stmtToDoc stmt in
  let str = pretty 40 dc in
    print_string str;
    print_string "\n\n\n";
  let stmt2 = get_prog_f filename in
  let dc2  = stmtToDocTest stmt2 in
  let str2 = pretty 40 dc2 in
    print_string str2;
    print_string "\n\n\n";
  true


let res = test_prog_2 "facten.imp"
let _ = print_string "\n\n"
let _ = if res then print_string "true" else print_string "false"



(*
###################
###########################################################
###########

###################
############################################################
############


*)

(* 20,6-8 *)
