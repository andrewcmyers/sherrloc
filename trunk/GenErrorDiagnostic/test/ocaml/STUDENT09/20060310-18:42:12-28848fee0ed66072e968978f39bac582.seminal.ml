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
(*let rec fits = Pprintdata.fits*)
let rec fits w imdList = 
  if w < 0 then
    false
  else
    match imdList with
      [] -> true
    | hd::tl -> 
	match hd with
	  (i, m, d) ->
	    match d  with
	    DocNil -> true
	|   DocText (str1)-> 
	    let len = String.length str1 in
	    if (len >w) then
	      false
	    else
	      fits (w-len) tl
	|   DocCons (doc1, doc2) -> fits w (i, m, doc1)::(i, m, doc2)::tl
	|   DocNest (len, doc1) -> fits (w+len) ((i, m, doc1)::tl)
	|   DocGroup (doc1) -> fits w ((i, m, doc1)::tl)
	|   DocBreak (str) ->
	    match m with
	      Break ->true
	    | Flat -> fits w tl
        
(* ################################################################
################################################################
####################################################################
###############################
#################################
############################################################## *)
let rec format w k imdList = 
  match imdList with
    [] -> SNil
  | hd::tl ->
      match hd with
	(i, m, d) ->
	  match d with
	    DocNil -> format w k tl
	  | DocCons (doc1, doc2) -> format w k ((i, m, doc1)::(i, m, doc2)::tl)
	  | DocNest (nestI, doc1) -> format w k ((i+nestI, m, doc1)::tl)
	  | DocText (str) -> SText(str, (format w (k+(String.length str)) tl)) 
	  | DocGroup (doc1) -> let doesFit =
	                         fits (w-k) ((i, Flat, doc1)::[]) in
	                         if (doesFit) then
				   format w k ((i, Flat, doc1)::tl)
				 else
				   format w k ((i, Break, doc1)::tl)
	  | DocBreak(str) ->
	      match m with
		Flat -> SText (str, (format w (k+(String.length str)) tl))
	      | Break -> SLine(i, (format w (k+(String.length str)) tl))
          
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

let wrap_paren doc =
  Pprintdata.group [Pprintdata.text "("; doc; Pprintdata.text ")"]

let rec stmtToDoc stmt = 
  match stmt with
  | Skip -> Pprintdata.text "skip\n"
  | Assign (s, e) -> Pprintdata.group [Pprintdata.text s; Pprintdata.text ":="; expToDoc e]
  | Seq (stmt1, stmt2) -> Pprintdata.group [stmtToDoc stmt1; Pprintdata.text ";\n"; stmtToDoc stmt2]
  | If (e, stmt1, stmt2) -> Pprintdata.group [Pprintdata.text "if "; expToDoc e; Pprintdata.text "\n"; wrap_paren (stmtToDoc stmt1); wrap_paren (stmtToDoc stmt2)]
  | While (e, s) -> Pprintdata.group [Pprintdata.text "while "; expToDoc e; Pprintdata.text "\n";wrap_paren (stmtToDoc s)]

  
type xmlAst = 
    Element of string                      (* ############### *)
    * (string * string) list  (* ######################################### *)
    * xmlAst list             (* ######################### *)

let rec getAttListDoc attList =
  match attList with
    [] -> Pprintdata.text ""
  | (str1,str2)::tl -> 
      let doc = Pprintdata.group [Pprintdata.text " "; Pprintdata.text str1; Pprintdata.text "="; Pprintdata.text str2;] in
      Pprintdata.group [doc; getAttListDoc tl]

let rec map f lst =
  match lst with
    []     -> []
  | hd::tl -> (f hd)::(map f tl)
		  
let rec xmlToDoc xml = 
    match xml with
      Element (eName, attList, xmlList) -> 
	let eDoc = Pprintdata.group [Pprintdata.text "<"; Pprintdata.text eName;] in
	let attDoc = getAttListDoc attList in
	let rec xmlDoc = Pprintdata.group [eDoc; attDoc; Pprintdata.text ">\n";] in
	    Pprintdata.group [xmlDoc; Pprintdata.group( map xmlToDoc xmlList);  Pprintdata.text "</"; Pprintdata.text eName; Pprintdata.text ">\n"]

(******************** pretty-printer test *************)
let get_prog_f filename =
  let ch = open_in filename in
  let res = Parse.program Lex.lexer (Lexing.from_channel ch) in
  close_in ch;
  res

let get_prog_s str = Parse.program Lex.lexer (Lexing.from_string str)


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

let _ =
  let arg1 = Sys.argv.(1) in 
  let arg2 = Sys.argv.(2) in
  let colCount = Int32.of_string arg2 in
  let _ = print_doc arg1 (Int32.to_int colCount) in
  let res = test_prog arg1 in
  match res with
    | true -> print_endline "true"
    | false -> print_endline "false"

let _ =
  let xmlArg = Element("top", [("first", "val1"); ("second", "val2")],
		       [Element ("next",[("foo", "bar")],[]);
		        Element ("no", [("way", "in")],[]);
                        Element ("hell", [], [])]) in
  let arg2 = Sys.argv.(2) in 
  let colCount = Int32.of_string arg2 in
  pretty_print (Int32.to_int colCount) (xmlToDoc xmlArg)

(* 48,29-66 *)
