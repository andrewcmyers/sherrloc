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

let rec stmtToDoc stmt = 
  match stmt with
  | Skip ->"skip"
  | Assign (s,e) -> s ^ "=" ^ (expToDoc e)
     

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
  let doc = get_doc_from_file filename in
  let doc_one_string = pretty 100 doc in
  let stmt_two = get_prog_s doc_one_string in
  let doc_two = stmtToDoc stmt_two in
  let doc_two_string = pretty 100 doc_two in
  let comp = String.compare doc_one_string doc_two_string in
  match comp with
      | 0 -> true
      | _ -> false


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

(* ########################### *)
let get_doc_from_file filename = 
  let ret = get_prog_f filename in 
  stmtToDoc ret

(* ################## *)
let print_doc fName colCount =
  let doc = get_doc_from_file fName in
  pretty_print colCount doc

let _ =
  let arg1 = Sys.argv.(1) in 
  let arg2 = Sys.argv.(2) in
  let colCount = Int32.of_string arg2 in
  let _ = print_doc arg1 (Int32.to_int colCount) in
  let res = test_prog arg1 in
  match res with
    | true -> print_endline "true"
    | false -> print_endline "false"
(* 70,30-42 *)
