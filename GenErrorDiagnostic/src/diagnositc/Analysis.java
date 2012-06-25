package diagnositc;

import java.io.FileReader;
import java.util.List;

import constraint.ast.Equation;
import constraint.graph.ConstraintGraph;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
	public static void main(String[] args) {
		try {
		    parser p = new parser(new GrmLexer(new FileReader("src/constraint/tests/test5.con")));
		    List<Equation> result = (List<Equation>) p.parse().value;
		    for (Equation e: result) {
		    	System.out.println( e.toString());
		    }
		    ConstraintGraph graph = new ConstraintGraph(result);
		    graph.writeToDotFile();
		  }
		  catch (Exception e) {e.printStackTrace();}
	}
}
