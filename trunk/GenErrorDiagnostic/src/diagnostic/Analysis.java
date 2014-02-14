package diagnostic;

import graph.ConstraintGraph;
import graph.ConstraintPath;
import graph.Edge;
import graph.Node;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.HTTPUtil;
import util.PrettyPrinter;
import constraint.analysis.PathFinder;
import constraint.analysis.ShortestPathFinder;
import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.Hypothesis;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

/**
 * The top level interface for error diagnosis
 */
public class Analysis implements PrettyPrinter {
	private DiagnosticOptions option;
	private ConstraintGraph graph;	// a constraint graph from constraints
	private UnsatPaths unsatPaths;	// a set of unsatisfiable paths from constraint analysis on graph

	/** internal states */
	private HashMap<Hypothesis, Hypothesis> cachedEnv;	// Reuse saturated hypothesis graph when possible
    private boolean DEBUG = false;    
    private boolean done = false;	
	HTTPUtil util;
	
	private Analysis(ConstraintGraph g, DiagnosticOptions option) {
		graph = g;
		this.option = option;
        unsatPaths = new UnsatPaths();
        util = new HTTPUtil();
        cachedEnv = new HashMap<Hypothesis, Hypothesis>();
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
	 * @param Input Constraint file
	 * @param Symmentric True if only equality is used in constraints
	 * @return An analysis instance
	 * @throws Exception
	 */
	static public Analysis getAnalysisInstance (String consFile, boolean isSym) throws Exception {
		DiagnosticOptions option = new DiagnosticOptions(consFile, isSym);
		
	    return getAnalysisInstance(option);
	}
	
	/**
	 * Return an instance of constraint analysis. Currently, the only analysis
	 * implemented is {@link ShortestPathFinder}
	 * 
	 * @return An constraint analysis algorithm
	 */
	public PathFinder getPathFinder () {
		return new ShortestPathFinder(graph);
	}
	
    public void genErrorPaths ( ) {        
        ArrayList<Node> startNodes = new ArrayList<Node>();
        ArrayList<Node> endNodes = new ArrayList<Node>();
        Set<Element> elements = graph.getAllElements();
        Set<Element> testElements = new HashSet<Element>();
                
        if (DEBUG) {
        	System.out.println("Total nodes before path generaton: " + elements.size());        
        }
        
        for (Element element : elements) {
            if (!(element instanceof JoinElement))                    
            	startNodes.add(graph.getNode(element));
			if (!(element instanceof MeetElement))
            	endNodes.add(graph.getNode(element));
        }
        
        if (DEBUG) {
        	System.out.println("Total start nodes before path generaton: " + startNodes.size());
        	System.out.println("Total end nodes before path generaton: " + endNodes.size());
        	System.out.println("Total comparison required: " + startNodes.size() * endNodes.size());
        }

    	PathFinder finder = getPathFinder();
    	if (option.isVerbose())
    		System.out.println("graph_size: "+graph.getAllNodes().size());
				
		for (Node start : startNodes) {
			for (Node end : endNodes) {
				Element e1 = start.getElement();
				Element e2 = end.getElement();
				
				if (option.isSymmetric() && (start.getIndex() <= end.getIndex()))
						continue;
				
				List<Edge> l = finder.getPath(start, end, option.isVerbose());
				if (l==null) continue;
								
				if (!option.isRecursive() && start.getIndex() != end.getIndex()) {
					if ( (e1 instanceof ConstructorApplication && e1.getVars().contains(e2)) 
					  || (e2 instanceof ConstructorApplication && e2.getVars().contains(e1))) {
						ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv(), cachedEnv);
						path.setCause();
						unsatPaths.addUnsatPath(path);
						continue;
					}
				}
				
				// if one end is variable, or an join/meet with a variable, the satisfiability is trivial
				if (e1.trivialEnd() || e2.trivialEnd()) {
					continue;
				}
								
				// less interesting paths
				if (e1.isBottom() || e2.isTop())
					continue;
								
				ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv(), cachedEnv);

				if (path.isSuccPath()) {
					path.incSuccCounter();
					continue;
				}								
				else if (path.isUnsatPath()) {
					testElements.add(e1);
					testElements.add(e2);
					path.setCause();
					unsatPaths.addUnsatPath(path);
					if (DEBUG)
						System.out.println(path);
				}
			}
		}
				
		done = true;
	}
    
    public int getPathNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	int ret = unsatPaths.size();
    	return ret;
    }
    
    public int getAssumptionNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	return (new MissingHypoInfer(unsatPaths)).getAssumptionNumber();
    }
    
    public String getAssumptionString () {
    	if (!done) {
    		genErrorPaths();
    	}
    	return (new MissingHypoInfer(unsatPaths)).getAssumptionString();
    }
        
    private void writeToDotFile () {
        String filename;

        filename = "error.dot";
        
        if (!done) 
        	genErrorPaths();
        
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
    
        if (!done) 
        	genErrorPaths();
        
        // out.write(getHeader());
        sb.append(HTTPUtil.getFeedback());
    	
    	sb.append( "<!-- ======== START OF ERROR REPORT ======== -->\n" +
    			"<H2>\n" +
    			"<BR>\n" +
    			"Error Diagnostic Report </H2>\n" +
    			"<HR>\n");
    	
    	// type check succeeded
    	if (unsatPaths.size()==0) {
    		sb.append("<H2>");
			sb.append("The program passed type checking. No errors were found.</H2>");
            sb.append("<script type=\"text/javascript\">"+
                      "document.getElementById('feedback').style.display = 'none';</script>");
    	}
    	else {
   			sb.append("<H2>One typing error is identified<H2>");
    		sb.append(getOneSuggestion(option.sourceName, unsatPaths));
    		sb.append(util.genAnnotatedCode(unsatPaths, option.sourceName) +
    				(option.sourceName.contains("jif")?("<script>colorize_all(); numberSuggestions();</script>\n")
    				:("<script>numberSuggestions();</script>\n")));
        }
    	
    	return sb.toString();
    }
                
    @Override
    public String toConsoleString () {
        if (!done) 
        	genErrorPaths();
        
        // type check succeeded
        if (unsatPaths.size()==0) {
			return ("The program passed type checking. No errors were found.");
		} else {
			return ("One typing error is identified\n" + getOneSuggestion(option.sourceName, unsatPaths));
		}
    }
    
    @Override
    public String toDotString() {
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
