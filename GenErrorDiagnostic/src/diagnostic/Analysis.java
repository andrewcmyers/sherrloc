
package diagnostic;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
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
import constraint.ast.ComplexElement;
import constraint.ast.Constraint;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.Position;
import constraint.ast.Variable;
import constraint.graph.ConstraintGraph;
import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.Node;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
    boolean DEBUG = false;
    boolean SHOW_WHOLE_GRAPH=false;
    boolean GEN_CUT = true;
    boolean GEN_ASSUMP = true;
	boolean done = false;
	ConstraintGraph graph;
	String sourceName;
	String htmlFileName;
	Position pos=null;
	Map<String, Node> exprMap;
	Map<String, Double> succCount;
	UnsatPaths unsatPaths;
	HTTPUtil util;
	public HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        exprMap = new HashMap<String, Node>();
        succCount = new HashMap<String, Double>();
        unsatPaths = new UnsatPaths();
        util = new HTTPUtil();
        cachedEnv = new HashMap<Environment, Environment>();
	}
	
	public static void main(String[] args) {
		
		Options options = new Options();
		options.addOption("f", false, "show full dependency graph");
		options.addOption("s", false, "symmetric");
		options.addOption("c", false, "generate cut");
		options.addOption("a", false, "generate assumptions");
		options.addOption("o", true, "output file");
		options.addOption("i", true, "original source file generating the constraints");
		
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
		
		boolean whole_graph = false;
		boolean symmentric = false;
		boolean cut = false;
		boolean assumption = false;
		String outfile = "error.html";
		String infile = "";
		
		if (cmd.hasOption("f"))		
			whole_graph = true;
		if (cmd.hasOption("s"))
			symmentric = true;
		if (cmd.hasOption("c"))
			cut = true;
		if (cmd.hasOption("a"))
			assumption = true;
		if (cmd.hasOption("o"))
			outfile = cmd.getOptionValue("o");
		if (cmd.hasOption("i"))
			infile = cmd.getOptionValue("i");
		
		for (Object arg : cmd.getArgList()) {
			String diagfile = (String) arg;
			try {
				Analysis ana = Analysis.getAnalysisInstance(diagfile, symmentric);
				ana.SHOW_WHOLE_GRAPH = whole_graph;
				ana.GEN_ASSUMP = assumption;
				ana.GEN_CUT = cut;
			    ana.sourceName = infile;
				ana.htmlFileName = outfile;
				ana.initialize();
				ana.writeToDotFile();
				ana.writeToHTML();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	private void initialize () {
		if (!graph.generated) 
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
//		return new ExistancePathFinder(this);
		return new ShortestPathFinder(g);
	}
	
    public void genErrorPaths ( ) {
        
        ArrayList<ElementNode> startNodes = new ArrayList<ElementNode>();
        ArrayList<ElementNode> endNodes = new ArrayList<ElementNode>();
        Set<Element> elements = graph.getAllElements();
        Set<Element> testElements = new HashSet<Element>();
        
        // initialize the maps, expr to node and succ path counter
        for (Node n : graph.getAllNodes()) {
        	exprMap.put(n.toString(), n);
        	succCount.put(n.toString(), 0.0);
        }
        
        if (DEBUG) {
        	System.out.println("Total nodes before path generaton: " + elements.size());        
        }
        
        for (Element element : elements) {
            if (element.isStart())                    
            	startNodes.add(graph.getNode(element));
			if (element.isEnd())
            	endNodes.add(graph.getNode(element));
        }
        
        if (DEBUG) {
        	System.out.println("Total start nodes before path generaton: " + startNodes.size());
        	System.out.println("Total end nodes before path generaton: " + endNodes.size());
        	System.out.println("Total comparison required: " + startNodes.size() * endNodes.size());
        }

    	PathFinder finder = getPathFinder( graph);
    	System.out.println("graph_size: "+graph.getAllNodes().size());
				
		for (ElementNode start : startNodes) {
			for (ElementNode end : endNodes) {
				Element e1 = start.getElement();
				Element e2 = end.getElement();
				
				if (graph.isSymmentric() && (start.getIndex() <= end.getIndex()))
					continue;
				
				// if one end is variable, the satisfiability is trivial
				if (e1 instanceof Variable || e2 instanceof Variable) {
					continue;
				}
				
				// also ignore the path if its satisfiability depends on other paths
				// DZ: not sure if this is necessary. Seems for OCaml, a constructor matches another is also an interesting information. Though 
				// this is less interesting for Jif labels, since they are the same constructors
				if (e1 instanceof ComplexElement && e2 instanceof ComplexElement) {
					if (((ComplexElement)e1).getCons().getBaseElement().equals(((ComplexElement)e2).getCons().getBaseElement()))
							continue;
				}
				
				// less interesting paths
				if (e1.isBottom() || e2.isTop())
					continue;
				
				List<Edge> l = finder.getPath(start, end);
				if (l==null) continue;
				
//				System.out.println("comparing "+e1 + " and " + e2);
				
				ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv());
		
//				// successful path
//				if (graph.getEnv().leq(e1, e2)) {
//					path.incSuccCounter();
//					continue;
//				}
				if (path.isSuccPath(cachedEnv)) {
					path.incSuccCounter();
					continue;
				}
								
//				System.out.println("***********");
//				for (Constraint c : env.getAssertions()) {
//					System.out.println(c);
//				}
//				System.out.println("***********");
//				System.exit(0);

				// successful path
//				if (env.leq(e1, e2)) {
//					path.incSuccCounter();
//					continue;
//				}
				else if (path.isUnsatPath(cachedEnv)) {
					testElements.add(e1);
					testElements.add(e2);
					path.incFailCounter();
					path.setCause();
					unsatPaths.addUnsatPath(path);
					System.out.println(path);
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
			double count = succCount.get(n.toString());
			succCount.put(n.toString(), count+n.getSuccCounter());
		}
		done = true;

	}
    
    public int getPathNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	int ret = unsatPaths.size();
//    	printRank();
    	return ret;
    }
    
    public int getAssumptionNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	return unsatPaths.getAssumptionNumber();
    }
    
    public String getAssumptionString () {
    	if (!done) {
    		genErrorPaths();
    	}
    	return unsatPaths.getAssumptionString();
    }
    
    void printRank () {    	
        Constraint[] all = graph.getConstraints().toArray(new Constraint[graph.getConstraints().size()]);
        Arrays.sort(all); 
        for (Constraint equ : all) {
            if (equ.getRank() >0) 
                System.out.println(equ.getRank() + ": " + equ.toString());
        }
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
        			"<H1>\n" +
        			"<BR>\n" +
        			"Error Diagnostic Report </H1>\n" +
        			"<HR>\n");
        	
        	// type check succeeded
        	if (unsatPaths.size()==0) {
        		out.append("<H2>");
    			out.append("The program passed type checking. No errors were found.</H2>");
                out.append("<script type=\"text/javascript\">"+
                          "document.getElementById('feedback').style.display = 'none';</script>");
        	}
        	else {
        		List<UnsatPaths> l = unsatPaths.genIndependentPaths();
        		if (l.size()==1) {
        			out.append("<H2>One typing error is identified<H2>");
        		}
        		else {
        			out.append("<H2>"+l.size()+" separate typing errors are identified</H2>");
        		}
        		int count = 1;	
        		for (UnsatPaths paths : l) {
        			System.out.println("***********"+paths.size());
        			out.append(getOneSuggestion(sourceName, paths, count++));
        		}
        		
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
    
    public String getOneSuggestion (String sourcefile, UnsatPaths paths, int count) {
    	StringBuffer sb = new StringBuffer();
   		sb.append(
    		"<HR>\n" +
    		"<H2>\n" +
    		"Error "+ count + "</H2>\n" +
    		paths.toHTML() +
    		(GEN_ASSUMP?paths.genMissingAssumptions(pos, sourcefile):"") +
//    		(GEN_CUT?paths.genElementCut():""));
    		(GEN_CUT?paths.genNodeCut(succCount, exprMap)+paths.genEdgeCut():""));
    	return sb.toString();
    }
}
