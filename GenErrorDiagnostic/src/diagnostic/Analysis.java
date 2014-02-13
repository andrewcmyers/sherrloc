
package diagnostic;

import graph.ConstraintGraph;
import graph.ConstraintPath;
import graph.Edge;
import graph.Node;
import graph.pathfinder.PathFinder;
import graph.pathfinder.ShortestPathFinder;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import util.HTTPUtil;
import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Position;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
    boolean DEBUG = false;
    boolean SHOW_WHOLE_GRAPH=false;
    boolean GEN_CUT = true;
    boolean GEN_ASSUMP = false;
    boolean GEN_UNIFIED = false;
    boolean REC = false;
    boolean VERBOSE = false;
	boolean done = false;
	ConstraintGraph graph;
	String sourceName;
	String htmlFileName;
	Position pos=null;
	Map<String, Integer> succCount;
	UnsatPaths unsatPaths;
	HTTPUtil util;
	public HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        succCount = new HashMap<String, Integer>();
        unsatPaths = new UnsatPaths();
        util = new HTTPUtil();
        cachedEnv = new HashMap<Environment, Environment>();
	}
	
	public static void main(String[] args) {
		
		Options options = new Options();
		options.addOption("a", false, "generate assumptions");
		options.addOption("c", false, "generate cut");
		options.addOption("d", false, "output constraint graph as a dot file");
		options.addOption("f", false, "show full dependency graph");
		options.addOption("i", true, "original source file generating the constraints");
		options.addOption("l", false, "console report");
		options.addOption("o", true, "output file");
		options.addOption("r", false, "allow recursion");
		options.addOption("s", false, "symmetric");
		options.addOption("u", false, "combined report with cut and assumptions");
		options.addOption("v", false, "report data (for evaluation)");
		
		CommandLineParser parser = new PosixParser();
		CommandLine cmd=null;
		try {
			cmd = parser.parse(options, args);
		}
		catch (ParseException e) {
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("diagnostic", options);
			System.exit(-1);
		}
		
		boolean assumption = false;
		boolean cut = false;
		boolean dotfile = false;
		boolean whole_graph = false;
		String infile = "";
		boolean locationonly = false;
		String outfile = "error.html";
		boolean recursion = false;
		boolean symmentric = false;
		boolean unified = false;
		boolean verbose = false;
		
		if (cmd.hasOption("a"))
			assumption = true;
		if (cmd.hasOption("c"))
			cut = true;
		if (cmd.hasOption("d"))
			dotfile = true;
		if (cmd.hasOption("f"))		
			whole_graph = true;
		if (cmd.hasOption("i"))
			infile = cmd.getOptionValue("i");
		if (cmd.hasOption("l"))
			locationonly = true;
		if (cmd.hasOption("o"))
			outfile = cmd.getOptionValue("o");
		if (cmd.hasOption("r"))
			recursion = true;
		if (cmd.hasOption("s"))
			symmentric = true;
		if (cmd.hasOption("u"))
			unified = true;
		if (cmd.hasOption("v"))
			verbose = true;
		
		for (Object arg : cmd.getArgList()) {
			String diagfile = (String) arg;
			try {
				Analysis ana = Analysis.getAnalysisInstance(diagfile, symmentric);
				ana.SHOW_WHOLE_GRAPH = whole_graph;
				ana.GEN_ASSUMP = assumption;
				ana.GEN_CUT = cut;
				ana.GEN_UNIFIED = unified;
				ana.REC = recursion;
				ana.VERBOSE = verbose;
			    ana.sourceName = infile;
				ana.htmlFileName = outfile;
				ana.initialize();
				if (dotfile) {
					ana.writeToDotFile();
				}
				if (locationonly)
					ana.toConsole();
				else
					ana.writeToHTML();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	private void initialize () {
		graph.generateGraph();
	}
	
	static public Analysis getAnalysisInstance (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new InputStreamReader(new FileInputStream(input), "UTF-8")));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;

	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    Analysis ret = new Analysis(graph);
	    ret.initialize();
	    return ret;
	}
	
	// this method is used to configure the path finder
	public PathFinder getPathFinder ( ConstraintGraph g) {
		return new ShortestPathFinder(g);
	}
	
    public void genErrorPaths ( ) {
        
        ArrayList<Node> startNodes = new ArrayList<Node>();
        ArrayList<Node> endNodes = new ArrayList<Node>();
        Set<Element> elements = graph.getAllElements();
        Set<Element> testElements = new HashSet<Element>();
        
        // initialize the maps, expr to node and succ path counter
        for (Node n : graph.getAllNodes()) {
        	succCount.put(n.toString(), 0);
        }
        
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

    	PathFinder finder = getPathFinder( graph);
    	if (VERBOSE)
    		System.out.println("graph_size: "+graph.getAllNodes().size());
				
		for (Node start : startNodes) {
			for (Node end : endNodes) {
				Element e1 = start.getElement();
				Element e2 = end.getElement();
				
				if (graph.isSymmentric() && (start.getIndex() <= end.getIndex()))
						continue;
				
				List<Edge> l = finder.getPath(start, end, VERBOSE);
				if (l==null) continue;
								
				if (!REC && start.getIndex() != end.getIndex()) {
					if ( (e1 instanceof ConstructorApplication && e1.getVars().contains(e2)) 
					  || (e2 instanceof ConstructorApplication && e2.getVars().contains(e1))) {
						ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv());
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
								
				ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv());

				if (path.isSuccPath(cachedEnv)) {
					path.incSuccCounter();
					continue;
				}								
				else if (path.isUnsatPath(cachedEnv)) {
					testElements.add(e1);
					testElements.add(e2);
					path.setCause();
					unsatPaths.addUnsatPath(path);
//					System.out.println(path);
				}
			}
		}
		
		for (ConstraintPath path : unsatPaths.getPaths()) {
			Environment env = new Environment();
			env.addEnv(path.getAssumption());
			env.addElements(testElements);
			path.setAssumption(env);
		}
		
		for (Node n : graph.getAllNodes()) {
			int count = succCount.get(n.toString());
			succCount.put(n.toString(), count+n.getSuccCounter());
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
        
    public void writeToDotFile () {
        String filename;

        filename = "error.dot";
        
        if (!done) 
        	genErrorPaths();
        
        try {
            FileOutputStream fstream = new FileOutputStream(filename);
            OutputStreamWriter out = new OutputStreamWriter(fstream,"UTF-8");
            if (SHOW_WHOLE_GRAPH) 
            	graph.labelAll();
            else
            	graph.slicing();
            out.write(graph.toDotString());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the DOT file to: " + filename);
        }
    }
    
    public void writeToHTML () {
        if (!done) 
        	genErrorPaths();
        
        try {
            FileOutputStream fstream = new FileOutputStream(htmlFileName);
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(fstream, "UTF-8"));
            // out.write(getHeader());
            writeFeedback(out);
        	
        	out.append( "<!-- ======== START OF ERROR REPORT ======== -->\n" +
        			"<H2>\n" +
        			"<BR>\n" +
        			"Error Diagnostic Report </H2>\n" +
        			"<HR>\n");
        	
        	// type check succeeded
        	if (unsatPaths.size()==0) {
        		out.append("<H2>");
    			out.append("The program passed type checking. No errors were found.</H2>");
                out.append("<script type=\"text/javascript\">"+
                          "document.getElementById('feedback').style.display = 'none';</script>");
        	}
        	else {
       			out.append("<H2>One typing error is identified<H2>");
        		out.append(getOneSuggestion(sourceName, unsatPaths, false));
        		out.append(util.genAnnotatedCode(unsatPaths, sourceName) +
        				(sourceName.contains("jif")?("<script>colorize_all(); numberSuggestions();</script>\n")
        				:("<script>numberSuggestions();</script>\n")));
            }
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the HTML file to: " + htmlFileName);
        }
    }
    
    public String getHeader () {
    	return "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n" +
    			"<!--NewPage-->\n" +
    			"<HTML>\n" +
    			"<HEAD>\n" +
    			"<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"utf-8\" />\n" +
    			"<TITLE>\n" +
    			"Experimental Error Diagnosis\n" +
    			"</TITLE>\n" +
    			"<SCRIPT type=\"text/javascript\">\n" +
    					"function windowTitle()\n" +
    					"{\n" +
    					"\tif (location.href.indexOf('is-external=true') == -1) {\n" +
    					"\t\tparent.document.title=\"report\";\n" +
    					"\t}\n" +
    					"}\n" +
    			"</SCRIPT>\n" +
    			
    			"\n<link rel=\"stylesheet\" href=\"style.css\">\n" +
    			"\n<script src=\"errors.js\"></script>\n" +
    			"\n<script src=\"colorize.js\"></script>\n" +
    			
    			"\n</HEAD>\n" +
    			
    			"\n<BODY BGCOLOR=\"white\" onload=\"windowTitle();\">\n" +
    			"\n";
    }
    
    public void writeFeedback(Writer out) throws IOException {
    	out.write("<div id=feedback class=feedback>\r\n");
    	out.write("<form method=\"POST\" action=\"submit.pl\" accept-charset=\"UTF-8\">");

    	out.write("Please rate this error diagnosis:<br>\r\n");

    	out.write("<input type=\"radio\" name=\"helpfulness\" value=\"1\"> 1. not helpful\r\n");
    	out.write("<input type=\"radio\" name=\"helpfulness\" value=\"2\"> 2. somewhat helpful\r\n");
    	out.write("<input type=\"radio\" name=\"helpfulness\" value=\"3\"> 3. helpful\r\n");
    	out.write("<input type=\"radio\" name=\"helpfulness\" value=\"4\"> 4. very helpful\r\n");
    	out.write("<input type=\"radio\" name=\"helpfulness\" value=\"5\"> 5. extremely helpful\r\n");
    	
    	out.write("<p>How does it compare in usefulness to the error message you get from OCaml?<br>\r\n");
    	out.write("<input type=\"radio\" name=\"comparison\" value=\"1\"> 1. much worse\r\n");
    	out.write("<input type=\"radio\" name=\"comparison\" value=\"2\"> 2. worse\r\n");
    	out.write("<input type=\"radio\" name=\"comparison\" value=\"3\"> 3. about the same\r\n");
    	out.write("<input type=\"radio\" name=\"comparison\" value=\"4\"> 4. better\r\n");
    	out.write("<input type=\"radio\" name=\"comparison\" value=\"5\"> 5. much better\r\n");
    	
    	out.write("<p>If you think you know where the error is in the program, please enter the line number:</p>");
    	out.write("<input type=\"text\" name=\"errorloc\" />");
    	
    	out.write("<p>If you have any other comments about how this diagnosis " +
    			"(or this tool) could be improved, you may enter them here:</p>\r\n");
    	out.write("<textarea name=\"comments\" rows=\"2\" cols=\"50\" /></textarea>\r\n");
    	out.write("<input type=\"submit\" value=\"Submit\"></div>\r\n");
    	out.write("</form>\r\n");
	    out.write("<button id=\"hide_button\" onclick=\"hide_feedback_form()\">show/hide</button>\r\n");
	
    	out.write("</div>\r\n");
    }
    
    public String getTail () {
    	return 	"\n\n" +
    			"</BODY>\n" +
    			"</HTML>";
    }
    
    public String toString () {
        if (!done) 
        	genErrorPaths();
        
        // type check succeeded
        if (unsatPaths.size()==0) {
			return "The program passed type checking. No errors were found.";
		} else {
			return "One typing error is identified\n" + getOneSuggestion(sourceName, unsatPaths, true);
		}
    }
    
    public void toConsole () {
    	System.out.println(toString());
    }
    
    public String getOneSuggestion (String sourcefile, UnsatPaths paths, boolean console) {
    	StringBuffer sb = new StringBuffer();
    	sb.append(
    		(GEN_ASSUMP?paths.genMissingAssumptions(pos, sourcefile):"") +
    		(GEN_CUT?(new ExprInfer(paths, succCount)).infer(console, VERBOSE)/*+paths.genEdgeCut()*/:""));
//    		(GEN_UNIFIED?paths.genCombinedResult(cachedEnv, exprMap, succCount):""));
    	if (!console) {             
    		sb.append("<HR>\n" + paths.toHTML());
        }

    	return sb.toString();
    }
}
