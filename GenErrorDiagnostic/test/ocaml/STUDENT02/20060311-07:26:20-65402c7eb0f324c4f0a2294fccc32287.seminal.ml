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

let rec printLen st len =
  if (len > 0) then (
    printLen (st ^ " ")  (len-1)
  ) else (
    st
  )
  
let rec sdocToString doc =
  match doc with
  | SNil        -> ""
  | SText(s,d)  -> s ^ (sdocToString d)
  | SLine(i,d)  ->
	  let chStr = printLen "" i in
	  "\n" ^ chStr ^ (sdocToString d)
 

(* #############################################################
#########################################
#############################################################################
########################################## *)
let rec fits w imdList = 
  if (w < 0) then (
    false
  ) else (
    match imdList with
      [] -> true
    | hd::tl ->
        match hd with
          (_, Break, DocBreak s)	-> true
        | (_, Flat, DocBreak s)		-> 
			fits w tl
        | (_, _, DocNil)			-> true
	    | (i,m,DocCons (dc1, dc2))	->
	        let dList = ((i, m, dc2)::tl) in
   	        let dList2 = ((i, m, dc1)::dList) in
   	        fits w dList2
	    | (i1,m,DocNest (i2, d))	->
	        let dList = ((0, m, d)::tl) in
	        fits (w-i2) dList 	
        | (_,_,DocText s)			->
		    fits (w-String.length(s)) tl
		| (i,_,DocGroup d)		-> 
		    let dList = ((i, Flat, d)::tl) in
		    fits (w-i) dList
  )


(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
let rec format w k imdList = 
  match imdList with
      []		-> SNil
    | hd::tl	->
    (
      match hd with
        (_,_,DocNil)			-> 
		  format w k tl
	  | (i,m,DocCons (dc1, dc2))-> (* ####################### *)
	      let dList = ((i, m, dc2)::tl) in
	      let dList2 = ((i, m, dc1)::dList) in
	      format w k dList2
	  | (i1,m,DocNest (i2, d))	->	(* ############################# *) 
		  let dList = ((i1+i2, m, d)::tl) in
		  format w k dList
	  | (_,_,DocText s)			-> 
		  let sD = format w (k+String.length(s)) tl in
		  SText( s, sD ) 
	  | (i, Flat, DocBreak s)		-> (* ################## *)
		  SText( s, (format w (k+String.length(s)) tl) )
	  | (i, Break, DocBreak s)		-> (* ################### *)
		  SLine( i, (format w (k+i) tl) ) 
	  | (i,_,DocGroup d)		-> 
	      let fitRes = fits w [i, Flat, d] in
	      if (fitRes = true) then (
	        let dList = ((i, Flat, d)::tl) in
			format w k dList  
		  ) else (
		    let dList = ((i, Break, d)::tl) in
		    format w k dList
		  )
    )


(* #################################################################
##################################################### *)
let pretty w doc =
  let sDc = format w 0 ([0, Flat, doc]) in 
  sdocToString sDc


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
  match xml with
    Element (s, s1, lst)		-> 
    (
      match s1 with
        (att1, att2)		->
          group [ nest 2 [text "<" ; br ; text s ; br ; ]  ]
    )
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






let test_prog_2 filename i =
  (* ######################################## *)
  let stmt = get_prog_f filename in
  let dc  = stmtToDoc stmt in
  let str = pretty i dc in
    print_string str;
    print_string "\n\n\n";

  let stmt2 = get_prog_f filename in
  let dc2  = stmtToDocTest stmt2 in
  let str2 = Pprintdata.pretty i dc2 in
    print_string str2;
    print_string "\n\n\n";
  true




let res = test_prog_2 "facten.imp" 60
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

(* 165,8-20 *)
