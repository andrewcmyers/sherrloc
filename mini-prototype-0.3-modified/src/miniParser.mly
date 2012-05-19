/**************************************************************************/
/*  Mini, a type inference engine based on constraint solving.            */
/*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               */
/*  and Didier Rémy.                                                      */
/*                                                                        */
/*  This program is free software; you can redistribute it and/or modify  */
/*  it under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation; version 2 of the License.               */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful, but   */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  General Public License for more details.                              */
/*                                                                        */
/*  You should have received a copy of the GNU General Public License     */
/*  along with this program; if not, write to the Free Software           */
/*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         */
/*  02110-1301 USA                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: miniParser.mly 421 2006-12-22 09:27:42Z regisgia $ */

%{

open CoreAlgebra
open MultiEquation
open Positions
open AstPositions
open MiniAst

let unclosed b e l1 l2 = 
  let l1 = lex_join (Parsing.rhs_start_pos l1) (Parsing.rhs_end_pos l1)
  and l2 = lex_join (Parsing.rhs_start_pos l2) (Parsing.rhs_end_pos l2)
  in
    raise (ParsingExceptions.Unclosed (b, e, l1, l2))
			    

let match_unit pos =
  PData (pos, DName "_Unit", [])

let app e1 e2 =
  EApp (joine e1 e2, e1, e2)

let infix (x,y) e1 e2 =
  app (app (EVar (x, SName y)) e1) e2

let seq e1 e2 =
  let pos = joine e1 e2 in
    EBinding (pos, BindValue (pos, [ pos, [], 
				     match_unit (position e1), e1 ]), e2)
     
let fold_pair f ts = 
  match ts with 
    | a :: b :: q -> List.fold_left f (f a b) q
    | _ -> assert false

let tuple2 pos t1 t2 = 
  EDCon (pos, DName "_Tuple", [ t1; t2 ])

let tuple pos = 
  fold_pair (tuple2 pos) 

let arrow_type pos t1 t2 =
  TypApp (pos, TypVar (pos, TName "->"), [ t1; t2 ])

let tuple_type2 pos t1 t2 = 
  TypApp (pos, TypVar (pos, TName "*"), [ t1; t2 ])

let tuple_type pos = 
  fold_pair (tuple_type2 pos)  

let ref_type pos t = 
  TypApp (pos, TypVar (pos, TName "ref"), [ t ])

let tuple_pat2 pos t1 t2 = 
  PData (pos, DName "_Tuple", [ t1 ; t2 ])

let tuple_pat pos = 
  fold_pair (tuple_pat2 pos) 

let assign pos e1 e2 = 
  EApp (pos, EApp (pos, EVar (pos, SName "_assign"), e1), e2)

let deref pos e = 
  EApp (pos, EVar (pos, SName "_deref"), e)

let mkref pos e = 
  EApp (pos, EVar (pos, SName "_ref"), e)

%}

%token <Positions.position * string> LID
%token <Positions.position * string> LONGID
%token <Positions.position * string> UID
%token <Positions.position * int> INTEGER
%token <Positions.position * char> CHAR
%token <Positions.position> LET
%token <Positions.position> VAL
%token <Positions.position> IN
%token <Positions.position> EQUAL
%token <Positions.position> BACKSLASH
%token <Positions.position> DOT
%token <Positions.position> LBRACE
%token <Positions.position> RBRACE
%token <Positions.position> LBRACKET
%token <Positions.position> RBRACKET
%token <Positions.position> LANGLE
%token <Positions.position> RANGLE
%token <Positions.position> TYPE
%token <Positions.position> EXISTS
%token <Positions.position> FORALL
%token <Positions.position> ARROW
%token <Positions.position> DARROW
%token <Positions.position> LPAREN
%token <Positions.position> RPAREN
%token <Positions.position> SEMI
%token <Positions.position> COMMA
%token <Positions.position> EOF
%token <Positions.position> BANG
%token <Positions.position> BAR
%token <Positions.position> ANDC
%token <Positions.position> COLON
%token <Positions.position> COERCE
%token <Positions.position> SLASH
%token <Positions.position> DATA
%token <Positions.position> REF
%token <Positions.position> MUTABLE
%token <Positions.position> LEFTARROW
%token <Positions.position> WILD
%token <Positions.position> AS
%token <Positions.position> REC
%token <Positions.position> AND
%token <Positions.position> MATCH
%token <Positions.position> WITH
%token <Positions.position> END
%token <Positions.position> ASSIGN
%token <Positions.position> BAR
%token <Positions.position> UNIT
%token <Positions.position> STAR
%token <Positions.position * string> INFIXOP0
%token <Positions.position * string> INFIXOP1
%token <Positions.position * string> INFIXOP2
%token <Positions.position * string> INFIXOP3
%token <Positions.position * string> INFIXOP4
%token <Positions.position> ASSERT_FALSE

%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4


%start program
%type <MiniAst.binding list> program

%%

program:
  bindings EOF                                      { List.rev $1 }
;

forall:
  /* epsilon */                                     { [] }
| FORALL quantifiers DOT                            { $2 }
;

quantifiers:
  quantifier                                        { [ $1 ] }
| quantifier quantifiers                            { $1 :: $2 }
;

quantifier:
  LID                                               { let p, s = $1 in
							p, TName s }
;

attributes: 
  typ						    { [], $1 }
| attribute SEMI attributes			    { $1 :: (fst $3), snd $3 }
;

attribute:
  LID COLON typ					    { LName (snd $1), $3 }
;

typ:
  type2                                             { $1 }
;

type2:
  type10 ARROW type2                                 
  { arrow_type (tjoin $1 $3) $1 $3  }
| type10                                             { $1 }
;

type10:
 star_types				            
{ match $1 with
    | [] -> assert false
    | [ a ] -> a
    | l -> tuple_type (tlposition $1) l
}
;

star_types:
  type1 STAR star_types                             { $1 :: $3 }
| type1						    { [ $1 ] }
;

type1:
  type0 { $1 }
| BACKSLASH type0				    { TypRowUniform ($1, $2) }
;

type0:
 type00s				    
  { 
    match $1 with
	[] -> assert false
      | [ t ] -> t
      | t :: q -> 
	  TypApp (join (tposition t)
		    (tlposition q),
		    t,
		    q) 
  }
| REF type0 { ref_type $1 $2 }
;

type00:
  LID                                               
  { TypVar (fst $1, TName (snd $1)) }
| LBRACE attributes RBRACE			    
      { let pos = join $1 $3 in
	TypApp (pos, TypVar (pos, TName "pi"), 
		[ TypRowCons (join $1 $3, fst $2, 
			      snd $2) ])
      }
| LPAREN typ RPAREN                                 { $2 }
| LPAREN typ COMMA types RPAREN                     { tuple_type (join $1 $5)
							($2 :: $4) }

;
/* TEMPORARY autoriser les 'equations inline dans les types */

type00s:
type00				            { [ $1 ] }
| type00 type00s			    { $1 :: $2 }
;

types:
  typ                                               { [ $1 ] }
| typ COMMA types                                   { $1 :: $3 }
;

expression:
  expression400                                     { $1 }
;

expression400:
  BACKSLASH pattern DOT expression400               
{ 
  ELambda (join $1 (position $4), $2, $4) 
}
| FORALL quantifiers DOT expression400 
{
  EForall (join $1 (position $4), snd (List.split $2), $4)
}
| EXISTS quantifiers DOT expression400 
{
  EExists (join $1 (position $4), snd (List.split $2), $4)
}
| MATCH expression WITH clauses END		    
{ 
  EMatch (join $1 $5, $2, List.rev $4)
} 
| binding IN expression400                          
{ 
  EBinding (join (bposition $1) (position $3), $1, $3) 
}
/* | FORALL quantifiers DOT expression400           { EForall ($2, $4) } 
    This production has been suppressed to avoid redundancy 
with the [let forall] construct.
    This decision simplifies the code that deals with polymorphic recursion. 
*/
| expression300 SEMI expression400                  { seq $1 $3 }
| expression300                                     { $1 }
;

clauses:
  clause					    { [ $1 ] }
| clauses BAR clause				    { $3 :: $1 }
;

clause:
  pattern DARROW expression			    { (join 
							 (pposition $1)
							 (position $3),
						       $1, $3) }
;

expression300:
  expression50 DOT LID LEFTARROW expression100      
  { 
    ERecordUpdate (joine $1 $5, $1, LName (snd $3), $5) 
  }
| expression200                                     { $1 }
;

expression200:
  expression200 INFIXOP0 expression200              { infix $2 $1 $3 }
| expression200 INFIXOP1 expression200              { infix $2 $1 $3 }
| expression200 INFIXOP2 expression200              { infix $2 $1 $3 }
| expression200 INFIXOP3 expression200              { infix $2 $1 $3 }
| expression200 INFIXOP4 expression200              { infix $2 $1 $3 }
| expression200 EQUAL expression200                 { infix ($2, "=") $1 $3 }
| expression100                                     { $1 }
;

expression100:
  expression100 expression0                         
  { 
    match $1 with
	EDCon (p, k, args) -> EDCon (p, k, args @ [ $2 ])
      | _ -> EApp (joine $1 $2, $1, $2) 
  }
| expression50                                      { $1 }
;

expression50:
  expression50 DOT LID                               
  { 
    ERecordAccess (join (position $1) (fst $3), $1, LName (snd $3))
  }
| expression10					     { $1 }
;

expression10:
  expression0					     { $1 }
| expression0 ASSIGN expression0		     { assign (joine $1 $3)
							 $1 $3 
						     }
| BANG expression0				     { deref 
							 (join $1 
							    (position $2))
							 $2 
						     }
| REF expression0				     { mkref 
							 (join $1 
							    (position $2))
							 $2 
						     }
;

expression0:
  LID                                                
{ 
  EVar (fst $1, SName (snd $1)) 
}
| UID						     
{ 
  EDCon (fst $1, DName (snd $1), []) 
}
| LONGID                                             
{ 
  EVar (fst $1, SName (snd $1))
}
| ASSERT_FALSE				             { EAssertFalse ($1) }
| INTEGER                                            { EPrimApp 
							 (fst $1, 
							  PIntegerConstant 
							    (snd $1), 
							  []) } 
| CHAR                                               { EPrimApp 
							 (fst $1, 
							  PCharConstant 
							    (snd $1), 
							  []) } 
| LBRACE record_bindings RBRACE                      { ERecordExtend (
							 join $1 $3,
							 List.rev $2,
							 ERecordEmpty undefined_position) 
						     }
| LPAREN RPAREN                                      { EPrimApp (
							 join $1 $2,
							 PUnit,
							 [])
						     }
| LPAREN expression RPAREN                           { $2 }
| LPAREN expression error
      {
	unclosed "(" ")" 1 3
      }

| LPAREN expression COLON typ RPAREN                 
    { ETypeConstraint (join $1 $5, $2, $4) 
}
| LPAREN expression COMMA expressions RPAREN         { tuple (join $1 $5) ($2 :: $4) }
;

expressions:
  expression                                         { [ $1 ] }
| expression COMMA expressions                       { $1 :: $3 }
;

record_bindings:
  /* epsilon */ { [] }
| record_binding { [ $1 ] }
| record_binding AND record_bindings { $1 :: $3 }
;

record_binding:
  LID EQUAL expression { (LName (snd $1), $3) }
;

bindings:
  /* epsilon */                                      { [] }
| bindings binding                                   { $2 :: $1 }
;

binding:
  LET value_definitions                              
  {
    BindValue (join $1 (vlposition $2), $2) 
  }
| LET REC value_definitions                          { BindRecValue 
							 (join $1 (vlposition $3),
							  $3) }
| TYPE type_definitions				     { TypeDec (join $1 (tdlposition $2), 
								$2) } 
;


type_definitions:
type_definition					     { [ $1 ] }
| type_definitions AND type_definition		     { $3 :: $1 }
;

type_definition:					
  LID COLON kind EQUAL forall algebraic_datatype_definitions   
  { 
    let alphas = snd (List.split $5) in
    let dcon = 
      List.map (fun (pos, lid, vars, ty) -> 
		(pos, lid, alphas @ vars, ty))
      $6
    in
      (fst $1, snd $3, TName (snd $1), DAlgebraic dcon) 
  }

| LID COLON kind 
  { (fst $1, snd $3, TName (snd $1), DAlgebraic []) }
;

kind:
  kind0 ARROW kind { (join (fst $1) (fst $3), KArrow (snd $1, snd $3)) }
| kind0 { $1 }
;

kind0: STAR { ($1, KStar) }
| LPAREN kind RPAREN { $2 }
;

algebraic_datatype_definitions:
  algebraic_datatype_definition			     { [ $1 ] }
| algebraic_datatype_definition BAR algebraic_datatype_definitions { $1 :: $3 }
;

algebraic_datatype_definition:
  UID COLON scheme				      
{ 
  (fst $1, DName (snd $1), snd (List.split (fst $3)), snd $3) 
}
;

scheme: forall typ { ($1, $2) }
;

value_definitions:
  value_definition                                    { [ $1 ] }
| value_definitions AND value_definition              { $3 :: $1 }
;

value_definition:
  forall pattern0 equal_expression                    { (position $3, 
							 snd (List.split $1), 
							 $2, $3) }
;

equal_expression:
  EQUAL expression                                    { $2 }
| COLON typ EQUAL expression                          
      { ETypeConstraint 
	  (join $1 (position $4), $4, $2) }
| pattern0 equal_expression                           { ELambda 
							  (join 
							     (pposition $1) 
							     (position $2), $1, $2) }
;

pattern:
  pattern3                                             { $1 }
;

pattern3:
  pattern2                                             { $1 }
| pattern3 COLON typ                                   
      { PTypeConstraint 
	  (join 
	     (pposition $1) 
	     (tposition $3), $1, $3) 
      }
;

pattern2:
  pattern1                                             { $1 }
| LID AS pattern2                                      
{ 
  PAlias (join (fst $1) (pposition $3), SName (snd $1), $3) 
}
;

pattern1:
  pattern10					       { $1 }
| pattern10 BAR pattern1				       { POr (pjoin $1 $3, [$1 ; $3]) }
| pattern10 ANDC pattern1			       { PAnd (pjoin $1 $3,[$1; $3]) }
;

pattern10:
  pattern0 { $1 } 
| UID pattern1s 
{ 
  PData (join (fst $1) (plposition $2), DName (snd $1), $2) 
}
;

pattern0:
  LID                                                   
{ 
  PVar (fst $1, SName (snd $1))
}
| UID 
{ 
  PData (fst $1, DName (snd $1), []) 
} 
| WILD                                                  { PWildcard $1 }
| INTEGER                                        { let pos = fst $1
						   and value = snd $1 in
						     PPrimitive 
						     (pos,
						      PIntegerConstant value) }
| CHAR                                        { let pos = fst $1
						   and value = snd $1 in
						     PPrimitive 
						     (pos,
						      PCharConstant value)
						 }
| LPAREN RPAREN                                         { match_unit $1 }
/* { tuple_pat (join $1 $2) [] } */
| LPAREN pattern RPAREN                                 { $2 }
| LPAREN pattern COMMA patterns RPAREN                  
{ tuple_pat (join $1 $5) 
    ($2 :: $4) }
;

patterns:
  pattern                                               { [ $1 ] }
| pattern COMMA patterns                                { $1 :: $3 }
;

pattern1s:
  pattern0						{ [ $1 ] }
| pattern0 pattern1s					{ $1 :: $2 }
;


