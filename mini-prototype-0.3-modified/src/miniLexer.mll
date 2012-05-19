(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: miniLexer.mll 421 2006-12-22 09:27:42Z regisgia $ *)

{

  open Lexing
  open MiniParser
  open Positions

  let comment_level = ref 0

(*------------------*
 | Error handling.  |
 *------------------*)

exception Error of string * int * int

let fail lexbuf message =
  raise (Error(message, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))
  
(*-----------------------*
 | Keyword recognition.  |
 *-----------------------*)

let table = 
  let t = Hashtbl.create 149 in
    List.iter (fun (keyword, token) ->
		 Hashtbl.add t keyword token
	      ) [
    "let", (fun x -> LET x);
    "val", (fun x -> VAL x);
    "in", (fun x -> IN x);
    "type", (fun x -> TYPE x);
    "forall", (fun x -> FORALL x);
    "data", (fun x -> DATA x);
    "mutable", (fun x -> MUTABLE x);
    "rec", (fun x -> REC x);
    "and", (fun x -> AND x);
    "exists", (fun x -> EXISTS x);
    "match", (fun x -> MATCH x);
    "with", (fun x -> WITH x);
    "end", (fun x -> END x);
    "as", (fun x -> AS x);
    "type", (fun x -> TYPE x);
    "ref", (fun x -> REF x);
  ] ;
    t

let filter lexbuf =
  let lid = (Lexing.lexeme lexbuf) in
    try
      (Hashtbl.find table lid) (cpos lexbuf)
    with Not_found ->
      LID (cpos lexbuf, lid)

}

(*---------*
 | Rules.  |
 *---------*)

let newline = 
  ('\010' | '\013' | "\013\010")
let blank =
  [' ' '\009' '\012']
let lowercase =
  ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase =
  ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1']*
let module_ident =
  uppercase identchar*
let path =
  (module_ident '.')* module_ident
let long_ident =
  path '.' lowercase identchar*

rule token = parse
  | newline { let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- 
		{ pos with 
		    pos_lnum = pos.pos_lnum + 1;
		    pos_bol  = pos.pos_cnum;
		};
		token lexbuf }
  | blank +
            { token lexbuf }
  | "_"     { WILD (cpos lexbuf) }
  | "assert false" { ASSERT_FALSE (cpos lexbuf) }
  | long_ident { LONGID (cpos lexbuf, Lexing.lexeme lexbuf) }
  | lowercase identchar *
            { filter lexbuf }
  | uppercase identchar *
            { UID (cpos lexbuf, Lexing.lexeme lexbuf) }
  | "\'"(_ as x)"\'" { CHAR (cpos lexbuf, x) }
  | decimal_literal | hex_literal | oct_literal | bin_literal
            { INTEGER (cpos lexbuf, int_of_string (Lexing.lexeme lexbuf)) }
  | "(*"
            { comment_level := 0; comment lexbuf; token lexbuf }

  | ","	    { COMMA (cpos lexbuf) }
  | "."     { DOT (cpos lexbuf)  }
  | "("     { LPAREN (cpos lexbuf)  }
  | ")"     { RPAREN (cpos lexbuf)  }
  | "{"	    { LBRACE (cpos lexbuf)  }
  | "}"     { RBRACE (cpos lexbuf)  }
  | "["	    { LBRACKET (cpos lexbuf)  }
  | "]"     { RBRACKET (cpos lexbuf)  }
  | "<-"    { LEFTARROW (cpos lexbuf)  }
  | "<"	    { LANGLE (cpos lexbuf)  }
  | ">"     { RANGLE (cpos lexbuf)  }
  | "->"    { ARROW (cpos lexbuf)  }
  | "=>"    { DARROW (cpos lexbuf)  }
  | "="     { EQUAL (cpos lexbuf)  }
  | ";"	    { SEMI (cpos lexbuf) }
  | "<:"     { COERCE (cpos lexbuf)  }
  | ":"     { COLON (cpos lexbuf)  }
  | "\\"    { BACKSLASH (cpos lexbuf)  }
  | "!"     { BANG (cpos lexbuf)  }
  | "/"     { SLASH (cpos lexbuf)  }
  | "|"     { BAR (cpos lexbuf)  }
  | "&"	    { ANDC (cpos lexbuf) }
  | "*"     { STAR (cpos lexbuf)  }
  | ":="    { ASSIGN (cpos lexbuf) }

  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0 (cpos lexbuf, Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1 (cpos lexbuf, Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2 (cpos lexbuf, Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4 (cpos lexbuf, Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3 (cpos lexbuf, Lexing.lexeme lexbuf) }

  | eof     { EOF (cpos lexbuf) }
  | _       { fail lexbuf "Illegal character" }

and comment = parse
  | "*)"
            { if !comment_level <> 0 then (decr comment_level; comment lexbuf) }
  | "(*"
            { incr comment_level; comment lexbuf }
  | eof
            { fail lexbuf "Unterminated comment" }

  | newline { let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- 
		{ pos with 
		    pos_lnum = pos.pos_lnum + 1;
		    pos_bol  = pos.pos_cnum;
		};
		comment lexbuf }
  | _
            { comment lexbuf }

