package diagnostic;

import graph.ConstraintGraph;
import graph.ConstraintPath;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import util.HTMLUtil;
import util.PrettyPrinter;
import constraint.analysis.ConstraintAnalysis;
import constraint.analysis.ConstraintAnalysisImpl;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

/**
 * The top level interface for error diagnosis
 */
public class Analysis implements PrettyPrinter {
	private DiagnosticOptions option;
	private ConstraintGraph graph;	// a constraint graph from constraints
	private ConstraintAnalysis cana;
	
	/** internal states */
	HTMLUtil util;
	
	private Analysis(ConstraintGraph g, DiagnosticOptions option) {
		graph = g;
		this.option = option;
        util = new HTMLUtil();
        cana = new ConstraintAnalysisImpl(option.isSymmetric(), option.isVerbose(), option.isRecursive());
	}

	/**
	 * Get an analysis instance from options
	 * 
	 * @param option Configurations
	 * @return An analysis instance
	 * @throws Exception
	 */
	static public Analysis getAnalysisInstance (DiagnosticOptions option) throws Exception {
	    parser p = new parser(new GrmLexer(new InputStreamReader(new FileInputStream(option.consFile), "UTF-8")));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;

	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints());
	    graph.generateGraph();
	    Analysis ret = new Analysis(graph, option);
	    return ret;
	}
	
	/**
	 * Get an analysis instance from a constraint file. Useful for unit tests
	 * 
	 * @param consFile Constraint file
	 * @param isSym True if only equality is used in constraints
	 * @return An analysis instance
	 * @throws Exception
	 */
	static public Analysis getAnalysisInstance (String consFile, boolean isSym) throws Exception {
		DiagnosticOptions option = new DiagnosticOptions(consFile, isSym);
		
	    return getAnalysisInstance(option);
	}
	
	public int getPathNumber() {
	 	return cana.genErrorPaths(graph).size();
	}
	       
    public int getAssumptionNumber () {
    	UnsatPaths paths = cana.genErrorPaths(graph);
    	return (new MissingHypoInfer(paths)).getAssumptionNumber();
    }
    
    public String getAssumptionString () {
    	UnsatPaths paths = cana.genErrorPaths(graph);
    	return (new MissingHypoInfer(paths)).getAssumptionString();
    }
        
    private void writeToDotFile () {
        String filename;

        filename = "error.dot";

        try {
            FileOutputStream fstream = new FileOutputStream(filename);
            OutputStreamWriter out = new OutputStreamWriter(fstream,"UTF-8");
            out.write(toDotString());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the DOT file to: " + filename);
        }
    }
    
    private void writeToHTML () {
        try {
            FileOutputStream fstream = new FileOutputStream(option.htmlFileName);
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(fstream, "UTF-8"));
            out.write(toHTMLString());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the HTML file to: " + option.htmlFileName);
        }
    }
    
    @Override
    public String toHTMLString() {
    	StringBuffer sb = new StringBuffer();
    
		UnsatPaths paths = cana.genErrorPaths(graph);
        
        // out.write(getHeader());
        sb.append(HTMLUtil.getFeedback());
    	
    	sb.append( "<!-- ======== START OF ERROR REPORT ======== -->\n" +
    			"<H2>\n" +
    			"<BR>\n" +
    			"Error Diagnostic Report </H2>\n" +
    			"<HR>\n");
    	
    	// type check succeeded
    	if (paths.size()==0) {
    		sb.append("<H2>");
			sb.append("The program passed type checking. No errors were found.</H2>");
            sb.append("<script type=\"text/javascript\">"+
                      "document.getElementById('feedback').style.display = 'none';</script>");
    	}
    	else {
   			sb.append("<H2>One typing error is identified<H2>");
    		sb.append(getOneSuggestion(option.sourceName, paths));
    		sb.append(util.genAnnotatedCode(paths, option.sourceName) +
    				(option.sourceName.contains("jif")?("<script>colorize_all(); numberSuggestions();</script>\n")
    				:("<script>numberSuggestions();</script>\n")));
        }
    	
    	return sb.toString();
    }
                
    @Override
    public String toConsoleString () {
		UnsatPaths paths = cana.genErrorPaths(graph);

        // type check succeeded
        if (paths.size()==0) {
			return ("The program passed type checking. No errors were found.");
		} else {
			return ("One typing error is identified\n" + getOneSuggestion(option.sourceName, paths));
		}
    }
    
    public String toDotString() {
    	UnsatPaths paths = cana.genErrorPaths(graph);
    	for (ConstraintPath path : paths.errPaths) {
    		path.setCause();
    	}
    	if (option.isWholeGraph()) 
        	graph.labelAll();
        else
        	graph.slicing();
        return graph.toDotString();
    }
    
    public String getOneSuggestion (String sourcefile, UnsatPaths paths) {
    	StringBuffer sb = new StringBuffer();
    	sb.append(
    		(option.isGenHypothesis()?paths.genMissingAssumptions(sourcefile):"") +
    		(option.isGenElements()?(new ExprInfer(paths, graph.getAllNodes())).infer(option.isToConsole(), option.isVerbose())/*+paths.genEdgeCut()*/:""));
//    		(GEN_UNIFIED?paths.genCombinedResult(cachedEnv, exprMap, succCount):""));
    	if (!option.isToConsole()) {             
    		sb.append("<HR>\n" + paths.toHTML());
        }

    	return sb.toString();
    }
    
    public void writeToOutput () {
		if (option.isDotFile()) {
			writeToDotFile();
		}
		if (option.isToConsole())
			System.out.println(toConsoleString());
		else
			writeToHTML();
    }
    
	/**
	 * Command line interface
	 */
	public static void main(String[] args) {

		DiagnosticOptions option = new DiagnosticOptions(args);

		try {
			Analysis ana = Analysis.getAnalysisInstance(option);
			ana.writeToOutput();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
