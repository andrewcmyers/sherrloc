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

(* ###### *)
let rec sdocToString doc = Pprintdata.sdocToString doc  
  (*match doc with
  | SNil        -> raise Unimplemented
  | SText(s,d)  -> raise Unimplemented
  | SLine(i,d)  -> raise Unimplemented*)

(* #############################################################
#########################################
#############################################################################
########################################## *)
(* #### *)
let rec fits w imdList = Pprintdata.fits w imdList

(* ############################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
(* #### *)
let rec format w k imdList = Pprintdata.format w k imdList

(* #################################################################
##################################################### *)
(* #### *)
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
	  Skip -> text "skip"
	| Assign(str, exp) ->
		nest 2 [group [text str ;
					   group [br ; text ":="] ] ;
					   group [br ; expToDoc exp] ]
	| Seq(stmt1, stmt2) ->
		group [stmtToDoc stmt1 ; group [breakWith "" ; text ";"] ; br ; stmtToDoc stmt2]
	| If(exp, stmt1, stmt2) ->
		conslist [nest 2 [group [text "if" ; nest 2 [br ; expToDoc exp] ; br] ;
				  group [text "(" ; nest 2 [br ; stmtToDoc stmt1 ; br] ; text ")" ; br] ;
				  group [text "(" ; nest 2 [br ; stmtToDoc stmt2 ; br] ; text ")"] ] ]
	| While(exp, stmt) ->
		conslist [group [text "while" ; br ; expToDoc exp ; br ; text "("] ;
				  nest 2 [br ; stmtToDoc stmt] ;
				  br ; text ")"]

type xmlAst = Element of 
	string                    (* ############### *)
    * (string * string) list  (* ######################################### *)
    * xmlAst list             (* ######################### *)

(* ################################# *)

let rec attrListToDoc attrList =
	let printAttr attr =
		match attr with
			(name, value) ->
				group [text name ; text "=" ; text "\"" ; text value ; text "\""]
	in match attrList with
		| [] -> br
		| hd::tl -> group [printAttr hd ; br ; attrListToDoc tl]
	
let rec xmlToDoc xml =
	let rec xmlListToDoc lst =
		match lst with
			| [] -> br
			| hd::tl -> xmlToDoc hd; xmlListToDoc tl		
	in match xml with
		Element(str, attributes, children) ->
			group [text "<" ; br ; text str ;
				   group [nest 2 [br ; attrListToDoc attributes ; text ">"] ] ;
				   group [nest 4 [br ; xmlListToDoc children] ] ;
				   text "</" ; br ; text str ; br ; text ">"]

(******************** pretty-printer test *************)

let get_prog_f filename =
  let ch = open_in filename in
  let res = Parse.program Lex.lexer (Lexing.from_channel ch) in
  close_in ch;
  res
  
let get_prog_s str = Parse.program Lex.lexer (Lexing.from_string str)

let test_prog filename =
	let w = 30 in
	if (pretty w (stmtToDoc (get_prog_f filename))
		= pretty w (stmtToDoc (get_prog_s (pretty w (stmtToDoc (get_prog_f filename))))))
		then true
		else false

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
  
(**************** Main - run the test ***************)

let main =
    if ((Array.length Sys.argv) < 3 )
    then print_string "Usage: pprint line_width filename"  
	else (
		print_string (linewidth (int_of_string Sys.argv.(1)));
		(*print_string (pretty (int_of_string Sys.argv.(1)) (stmtToDoc (get_prog_f Sys.argv.(2))))*)
		print_string (pretty (int_of_string Sys.argv.(1)) (xmlToDoc (get_prog_f Sys.argv.(2))))
	)
	
	
	(*(
		if (test_prog Sys.argv.(2))
		then print_string "Successful test!"
		else print_string "Unsuccessful test"
	)*)
(* xmlToDoc should be stmtToDoc, identified as the second suggestion *)
(* 152,53-61 *)
