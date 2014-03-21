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

let rec sdocToString doc =
  match doc with
  | SNil        -> raise Unimplemented
  | SText(s,d)  -> raise Unimplemented
  | SLine(i,d)  -> raise Unimplemented


(* #############################################################
#########################################
#############################################################################
########################################## *)
let rec fits w imdList = 
  raise Unimplemented

(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
let rec format w k imdList = 
  raise Unimplemented

(* #################################################################
##################################################### *)
let pretty w doc = Pprintdata.pretty w doc

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

let rec stmtToDoc stmt = Pprintdata.stmtToDoc stmt

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

let test_prog filename width = 
  let ret = get_prog_f filename in 
    let doc = stmtToDoc ret in
      pretty_print width doc


let _ =
let fName = Sys.argv.(1) in
  let colCount = Sys.argv.(2) in
    test_prog fName colCount

(********************* Other ***********************)

(* #######################################################################
################################### *)
let linewidth n = 
  let strn = string_of_int(n) in
  let charsstrn = String.length strn in
  let n = n-2-charsstrn in
  let (left, right) = if (n mod 2 = 0) 
                      then (n/2, n/2)
                      else ((n+1)/2,(n-1)/2) in
  "<" ^ (String.make left '-') ^ strn ^ (String.make right '-') ^ ">\n"
(* 94,20-28 *)
