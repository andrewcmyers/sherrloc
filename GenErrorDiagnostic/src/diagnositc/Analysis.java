package diagnositc;

import java.io.FileReader;

import constraint.graph.ConstraintGraph;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
	
	public static void main(String[] args) {
		try {
			ConstraintGraph graph = Analysis.getConstraintGraph("src/constraint/tests/jif/constant.con", false);
			graph.writeToDotFile();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	static public ConstraintGraph getConstraintGraph (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;
//	    for (Equation e: result) {
//	    	System.out.println( e.toString());
//	    }
	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    result.getEnv().printAssertions();
	    return graph;
	}
}
