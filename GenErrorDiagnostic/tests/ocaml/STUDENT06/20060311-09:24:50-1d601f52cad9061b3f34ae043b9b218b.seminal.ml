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

let rec sdocToString ssdoc = (*Pprintdata.sdocToString ssdoc*)
  match ssdoc with
  | SNil        -> ""
  | SText(s,d)  -> s ^ sdocToString d
  | SLine(i,d)  -> "\n" ^ String.make i  ' ' ^ sdocToString d

(* #############################################################
#########################################
#############################################################################
########################################## *)
(* #### *)
let rec fits w imdList = Pprintdata.fits w imdList

(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
(* #### *)
let rec format w k imdList = (*Pprintdata.format w k imdList*)
	match imdList with
	| []		-> SNil
	| hd::tl	-> 
		match hd with
		| (i,m,DocNil)			-> format w k tl
		| (i,m,DocCons(d1,d2))	-> format w k [(i, m, d1)::(i, m, d2)::tl]
		| (i1,m,DocNest(i2,d))	-> format w k [(i1 + i2, m, d)::tl]
		| (i,m,DocText(s))		-> SText(s, (format w (k + String.length s) tl))
		| (i,m,DocBreak(s))		->
			match m with
			| Flat	-> SText(s, (format w (k + String.length s) tl))
			| Break	-> SLine(i, (format w i tl))
		| (i,m,DocGroup(d))		->
			if (fits (w - k) [(i, Flat, d)::tl])
			then (format w k [(i, Flat, d)::tl])
			else (format w k [(i, Break, d)::tl])

(* #################################################################
##################################################### *)
let pretty w doc =
	sdocToString (format w 0 [(0, Flat, doc)])

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
	| Skip					-> text "skip"
	| Assign(str, exp)		->
		nest 2 [group [text str ;
					   group [br ; text ":="] ] ;
					   group [br ; expToDoc exp] ]
	| Seq(stmt1, stmt2)		->
		group [stmtToDoc stmt1 ; group [breakWith "" ; text ";"] ; br ; stmtToDoc stmt2]
	| If(exp, stmt1, stmt2)	->
		conslist [nest 2 [group [text "if" ; nest 2 [br ; expToDoc exp] ; br] ;
				  group [text "(" ; nest 2 [br ; stmtToDoc stmt1 ; br] ; text ")" ; br] ;
				  group [text "(" ; nest 2 [br ; stmtToDoc stmt2 ; br] ; text ")"] ] ]
	| While(exp, stmt)		->
		conslist [group [text "while" ; br ; expToDoc exp ; br ; text "("] ;
				  nest 2 [br ; stmtToDoc stmt] ;
				  br ; text ")"]

type xmlAst = Element of 
	string                    (* ############### *)
    * (string * string) list  (* ######################################### *)
    * xmlAst list             (* ######################### *)

let rec attrListToDoc attrList =
	let printAttr attr =
		match attr with
			(name, value) ->
				group [text name ;
					   breakWith "" ;
					   text "=" ;
					   breakWith "" ;
					   group [text "\"" ; breakWith "" ; text value ; breakWith "" ; text "\""] ]
	in match attrList with
	| []		-> empty
	| hd::tl	-> group [printAttr hd ; br ; attrListToDoc tl]
	
let rec xmlToDoc xml =
	let rec xmlListToDoc lst =
		match lst with
		| []		-> empty
		| hd::tl	-> conslist [xmlToDoc hd; xmlListToDoc tl]		
	in match xml with
		Element(str, attributes, children) ->
			group [group [group [text "<" ; breakWith "" ; text str] ;
						  nest 2 [br ; group [attrListToDoc attributes] ] ;
						  text ">"] ;
				   group [nest 4 [breakWith "" ; xmlListToDoc children] ] ;
				   breakWith "" ;
				   group [text "</" ; breakWith "" ; text str ; breakWith "" ; text ">"] ]

(******************** pretty-printer test *************)

let get_prog_f filename =
  let ch = open_in filename in
  let res = Parse.program Lex.lexer (Lexing.from_channel ch) in
  close_in ch;
  res
  
let get_prog_s str = Parse.program Lex.lexer (Lexing.from_string str)

let test_prog w filename =
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
		if (test_prog (int_of_string Sys.argv.(1)) Sys.argv.(2))
		then print_string "Successful test!"
		else print_string "Unsuccessful test"
	)

(* at least 5 errors, [] should be () *)
(* 45,40-83  46,40-76  53,21-37  54,21-37  55,21-37 *)
