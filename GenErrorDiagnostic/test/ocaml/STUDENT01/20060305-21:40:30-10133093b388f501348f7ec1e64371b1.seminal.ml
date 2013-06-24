open Pprintdata

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
let rec charString character length =
  match length with
  | 0 -> ""
  | _ -> character ^ charString character (length-1)

let rec sdocToString doc =
  match doc with
  | SNil        -> ""
  | SText(s,d)  -> s ^ (sdocToString d)
  | SLine(i,d)  -> "\n" ^ (String.make i ' ') ^ (sdocToString d)

(* #############################################################
#########################################
#############################################################################
########################################## *)
let rec fits w imdList =
   if w < 0 then false
   else 
     match imdList with
       []                         -> true
     | (indent, mode, doc)::tl    -> 
         match doc with 
           DocNil        -> true
         | DocText(s)    -> fits (w - (String.length s)) tl
         | DocBreak(s)   -> if mode = Break then true else fits (w - (String.length s)) tl
         | DocCons(d1,d2)-> fits w ((indent, mode, d1)::(indent, mode, d2)::tl)
         | DocGroup(d)   -> fits w ((indent, Flat, d)::tl)
         | DocNest(i,d)  -> fits w ((indent+i, mode, d)::tl)

(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
let rec format w k imdList = 
  match imdList with
    [] -> SNil
  | (indent, mode, doc)::tl -> 
      match doc with
        DocNil          -> format w k tl
      | DocText(s)      -> SText(s, format w (k + (String.length s)) tl)
      | DocBreak(s)     -> 
          if mode = Flat then 
            (SText(" ", format w (k + (String.length s)) tl)) 
          else 
            (SLine(indent, format w indent tl))
      | DocCons(d1,d2)  -> format w k ((indent, mode, d1)::(indent, mode, d2)::tl)
      | DocGroup(d)     -> 
          if (fits (w - k) imdList) then 
            format w k ((indent, Flat,  d)::tl)
          else 
            format w k ((indent, Break, d)::tl)
      | DocNest(i,d)    -> format w k ((indent + i, mode, d)::tl)

(* #################################################################
############################################################################# *)
let pretty w doc = 
  let sdoc = format w 0 [(0, Flat , doc)] in
  sdocToString sdoc

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
    Skip            -> text "skip"
  | Assign(str,e)   -> group [nest 2 [text str; br; text ":="; br; expToDoc e ]]
  | Seq(st1,st2)    -> group [stmtToDoc st1; text ";"; br; stmtToDoc st2;]
  | If(e,st1,st2)   -> group [group [nest 2 [text "if"; br; expToDoc e]]; br; 
                       group [nest 2 [text "("; br; stmtToDoc st1; br; text ")"]]; br;
                       group [nest 2 [text "("; br; stmtToDoc st2; br;  text ")" ]]]
  | While(e,st)     -> group [group [nest 2 [text "while"; br; expToDoc e]]; br;
                       group [nest 2 [text "("; br; stmtToDoc st; ]]; br; text ")"]

type xmlAst = Element of 
  string                      (* ############### *)
    * (string * string) list  (* ######################################### *)
    * xmlAst list             (* ######################### *)

(* ##################### *)
let xmlToDoc xml = 
  match xml with
    Element(elementName, (attName, attValue), tl) -> print_string "got elementName"
        

(******************** pretty-printer test *************)
let get_prog_f filename =
  let ch = open_in filename in
  let res = Parse.program Lex.lexer (Lexing.from_channel ch) in
  close_in ch;
  res
let get_prog_s str = Parse.program Lex.lexer (Lexing.from_string str)

(* ##################### *)
let test_prog filename = 
	let stuff = get_prog_f filename in 
	let doc = stmtToDoc stuff in
	pretty_print (int_of_string Sys.argv.(2)) doc

let _ = 
	if (Array.length Sys.argv) < 3 
	then print_string "Usage: pprint (filename) (width)" 
	else let width = (int_of_string Sys.argv.(2)) in
	print_string (linewidth width); 
	test_prog Sys.argv.(1)

(* constructor is missing *)
(* 133,25-44 *)
