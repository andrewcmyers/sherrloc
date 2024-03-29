package sherrloc.constraint.parse;
import java_cup.runtime.*;
            
%%

%public
%class GrmLexer
%cup
%unicode
%line
%column 

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }
%}

LineBreak		= \r|\n|\r\n
WhiteSpace     	= {LineBreak} | [ \t\f]
InputCharacter = [^\r\n]

Identifier = ([:jletter:]| "*") ([:jletterdigit:])* | ([:jletter:]| "*") ([:jletterdigit:] | - | > | "*" | ":" | "&" | "." | "{" | "}")+
DecIntegerLiteral = 0 | [1-9][0-9]*

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} 
TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment     = "//" {InputCharacter}* {LineBreak}

%state INFO

%% 
/* keywords */

<YYINITIAL> "CONSTRUCTOR"           { return symbol(sym.CONSTRUCTOR); }
<YYINITIAL> "FUNCTION"              { return symbol(sym.FUNCTION); }
<YYINITIAL> "VARIABLE"              { return symbol(sym.VARIABLE); }
<YYINITIAL> "MEET"           		{ return symbol(sym.MEET); }
<YYINITIAL> "meet"           		{ return symbol(sym.MEET); }
<YYINITIAL> "\u2293" 				{ return symbol(sym.MEET); }        /* ⊓ */
<YYINITIAL> "JOIN"           		{ return symbol(sym.JOIN); }
<YYINITIAL> "join"           		{ return symbol(sym.JOIN); }
<YYINITIAL> "\u2294" 				{ return symbol(sym.JOIN); }        /* ⊔ */
<YYINITIAL> "%%"					{ return symbol(sym.SEPERATION); }
<YYINITIAL> "AXIOM"                { return symbol(sym.AXIOM); }
<YYINITIAL> "axiom"                { return symbol(sym.AXIOM); }
<YYINITIAL> "level"                { return symbol(sym.LEVEL); }

<YYINITIAL> {
  	/* identifiers */ 
  	{Identifier}                   { return symbol(sym.IDENTIFIER, yytext()); }
 
  	/* literals */
  	{DecIntegerLiteral}            { return symbol(sym.INTEGER_LITERAL, Integer.parseInt(yytext())); }
  	"\""                           { string.setLength(0); yybegin(INFO); }

  	/* operators */
  	"=="                           { return symbol(sym.EQ); }
  	"<="                           { return symbol(sym.LEQ); }
  	">="                           { return symbol(sym.GEQ); }
  	"->"                           { return symbol(sym.ARROW); }
  	"<-"						   { return symbol(sym.LARROW); }
  	";"    						   { return symbol(sym.SEMICOLON); }
  	","							   { return symbol(sym.COMMA); }
  	"."                            { return symbol(sym.DOT); }
  	":"							   { return symbol(sym.COLON); }
  	"@"							   { return symbol(sym.AT); }
  	"=>"                           { return symbol(sym.IMPLIES); }
  	
  	/* parentheses */
  	"("    						   { return symbol(sym.LPAREN); }
  	")"							   { return symbol(sym.RPAREN); }
  	"["							   { return symbol(sym.LBRACK); }
  	"]"							   { return symbol(sym.RBRACK); }
  	"-"							   { return symbol(sym.DASH); }
  	"{"							   { return symbol(sym.LBRACE); }
 	"}"							   { return symbol(sym.RBRACE); }
  	
 	/* whitespace */
  	{WhiteSpace}                   { /* ignore */ }
  	
  	/* comments */
	{Comment}                      { /* ignore */ }
}

/* INFO associated with token */
<INFO> {
  "\""                             { yybegin(YYINITIAL); 
                                     return symbol(sym.STRING_LITERAL, 
                                     string.toString()); }
  [^\n\r\"\\]+                     { string.append( yytext() ); }
  "\\t"                            { string.append('\t'); }
  "\\n"                            { string.append('\n'); }

  "\\r"                            { string.append('\r'); }
  "\\\""                           { string.append('\"'); }
  "\\"                             { string.append('\\'); }
}


/* error fallback */
.|\n                             { throw new Error("Illegal character <"+ yytext()+">"+" at line "+yyline+" column "+yycolumn); }
