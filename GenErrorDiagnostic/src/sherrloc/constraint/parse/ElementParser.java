package sherrloc.constraint.parse;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import sherrloc.constraint.ast.Element;
import java_cup.runtime.*;

public class ElementParser {
    public static Element parse(String s) {
        try {
            parser p = new parser(new ElementLexer(s));
            return (Element) p.parse().value;
        } catch (Exception e) {
            return null;
        }
    }

    private static class ElementLexer implements Scanner {
        private boolean first = true;
        private final GrmLexer lexer;
        public ElementLexer(String s) {
            lexer = new GrmLexer(new StringReader(s));
        }

        @Override
        public Symbol next_token() throws IOException {
            if (first) {
                first = false;
                return new Symbol(sym.PARSE_ELEMENT);
            }
            return lexer.next_token();
        }
    }
}
