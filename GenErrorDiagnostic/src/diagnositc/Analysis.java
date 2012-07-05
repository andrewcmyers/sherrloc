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
			ConstraintGraph graph = Analysis.getConstraintGraph("src/constraint/tests/jif/array.con", false);
			graph.writeToDotFile();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	static public ConstraintGraph getConstraintGraph (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    List<Equation> result = (List<Equation>) p.parse().value;
//	    for (Equation e: result) {
//	    	System.out.println( e.toString());
//	    }
	    ConstraintGraph graph = new ConstraintGraph(result, symmentric);
	    return graph;
	}
}
