package constraint.parse;
import java_cup.runtime.*;
            
%%

%public
%class GrmLexer
%cup

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

Identifier = [:jletter:] [:jletterdigit:]*
DecIntegerLiteral = 0 | [1-9][0-9]*

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} 
TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment     = "//" {InputCharacter}* {LineBreak}

%state INFO

%% 
/* keywords */

<YYINITIAL> "CONSTRUCTOR"           { System.out.println("cons"); return symbol(sym.CONSTRUCTOR); }
<YYINITIAL> "JOIN"           		{ System.out.println("join"); return symbol(sym.JOIN); }
<YYINITIAL> "MEET"           		{ System.out.println("meet"); return symbol(sym.MEET); }


<YYINITIAL> {
  	/* identifiers */ 
  	{Identifier}                   { System.out.println(yytext()); return symbol(sym.IDENTIFIER, yytext()); }
 
  	/* literals */
  	{DecIntegerLiteral}            { return symbol(sym.INTEGER_LITERAL, new Integer(yytext())); }
  	"\""                           { string.setLength(0); yybegin(INFO); }

  	/* operators */
  	"="                            { return symbol(sym.EQ); }
  	"<="                           { System.out.println("<="); return symbol(sym.LEQ); }
  	">="                           { return symbol(sym.GEQ); }
  	"->"                           { return symbol(sym.ARROW); }
  	";"    						   { return symbol(sym.SEMICOLON); }
  	"*"							   { return symbol(sym.STAR); }
  	
  	/* parentheses */
  	"("    						   { return symbol(sym.LPAREN); }
  	")"							   { return symbol(sym.RPAREN); }
  	"@"							   { return symbol(sym.AT); }
  	
 	/* whitespace */
  	{WhiteSpace}                   { /* ignore */ }
  	
  	/* comments */
	{Comment}                      { /* ignore */ }
}

/* INFO associated with token */
<INFO> {
  "\""                           { yybegin(YYINITIAL); 
                                   return symbol(sym.STRING_LITERAL, 
                                   string.toString()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  "\\t"                            { string.append('\t'); }
  "\\n"                            { string.append('\n'); }

  "\\r"                            { string.append('\r'); }
  "\\\""                           { string.append('\"'); }
  "\\"                             { string.append('\\'); }
}


/* error fallback */
.|\n                             { throw new Error("Illegal character <"+ yytext()+">"); }