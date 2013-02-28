
package diagnostic;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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

import util.AttemptGoal;
import constraint.ast.ComplexElement;
import constraint.ast.Constraint;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.ast.Position;
import constraint.ast.Variable;
import constraint.graph.ConstraintGraph;
import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.EquationEdge;
import constraint.graph.Node;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
    boolean DEBUG = false;
    boolean SHOW_WHOLE_GRAPH=true;
    boolean GEN_CUT = true;
    boolean GEN_ASSUMP = true;
    int REC_MAX = 3;
	boolean done = false;
	ConstraintGraph graph;
	HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	String sourceName;
	String htmlFileName;
	Position pos=null;
	Map<String, Node> exprMap;
	Map<String, Double> succCount;
	UnsatPaths unsatPaths;
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        cachedEnv = new HashMap<Environment, Environment>();
        exprMap = new HashMap<String, Node>();
        succCount = new HashMap<String, Double>();
        unsatPaths = new UnsatPaths();
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
//				ana.writeToDotFile();
				ana.writeToHTML();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	static public Analysis getAnalysisInstance (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;

	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    Analysis ret = new Analysis(graph);
	    return ret;
	}
	
	// this method is used to configure the path finder
	public PathFinder getPathFinder ( ConstraintGraph g) {
//		return new ExistancePathFinder(this);
		return new ShortestPathFinder(g);
	}
	
    public void genErrorPaths ( ) {
        if (!graph.generated) 
        	graph.generateGraph();
        
        ArrayList<ElementNode> startNodes = new ArrayList<ElementNode>();
        ArrayList<ElementNode> endNodes = new ArrayList<ElementNode>();
        Set<Element> elements = graph.getAllElements();
        
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
				
				List<Edge> l = finder.getPath(start, end);
				if (l==null) continue;
				
//				System.out.println("comparing "+e1 + " and " + e2);
				
				// also ignore the path if its satisfiability depends on other paths
				if (e1 instanceof ComplexElement && e2 instanceof ComplexElement) {
					if (((ComplexElement)e1).getCons().sameas(((ComplexElement)e2).getCons()))
							continue;
				}
				
				ConstraintPath path = new ConstraintPath(l, finder);

				// successful path
				if (graph.getEnv().leq(e1, e2)) {
					path.incSuccCounter();
					continue;
				}
				
				// if failed, try to use the assumptions on path
				Environment env;
				if (cachedEnv.containsKey(path.getAssumption()))
					env = cachedEnv.get(path.getAssumption());
				else {
					env = new Environment();
					env.addEnv(graph.getEnv());
					env.addEnv(path.getAssumption());
					cachedEnv.put(path.getAssumption(), env);
				}

				// successful path
				if (env.leq(e1, e2)) {
					path.incSuccCounter();
					continue;
				}
//				System.out.println(path.toString());
//				System.out.println(path.getEdges().size());
				path.incFailCounter();
				path.setCause();
				AttemptGoal goal = new AttemptGoal(start.getElement(), end.getElement(), env);
				unsatPaths.addUnsatPath(goal, path);
			}
		}
		
		for (Node n : graph.getAllNodes()) {
			double count = succCount.get(n.toString());
			succCount.put(n.toString(), count+n.getSuccCounter());
		}
		done = true;

	}
    
    public String getOneSuggestion (String sourcefile, HashMap<AttemptGoal, ConstraintPath> errorPaths, int count) {
    	StringBuffer sb = new StringBuffer();
   		sb.append(
    		"<HR>\n" +
    		"<H2>\n" +
    		"Error "+ count + "</H2>\n" +
   			unsatPathsToHTML(errorPaths) +
    		genMissingAssumptions(errorPaths) +
    		genCutItems(errorPaths));
    	return sb.toString();
    }
    
    public String unsatPathsToHTML (Map<AttemptGoal, ConstraintPath> errorPaths) {
    	StringBuffer sb = new StringBuffer();
    	sb.append("<H3>");
    	sb.append(errorPaths.size() +" type mismatch" + (errorPaths.size() == 1 ? "" : "s") + " found: \n");
		sb.append("</H3>\n");
		sb.append("<UL>\n");
		for (AttemptGoal goal : errorPaths.keySet()) {
			//sb.append("<div class=\"moreinfo\"> " +
			//		errorPaths.get(goal).toString() + " \n");
			//sb.append("</div>);
			StringBuffer path_buff = new StringBuffer();
			List<Node> nodes = errorPaths.get(goal).getIdNodes();
			for (Node n : nodes) {
				path_buff.append("['pathelement', \'"+((ElementNode)n).getElement().getPosition()+"\'], ");
			}
			sb.append("<LI>\n<span class=\"path\" ");
			setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">");
			sb.append("A value with type "+goal.getSource().toDetailString() + 
				    " is being used at type " + goal.getSink().toDetailString());
			sb.append("</span>\n");
        		sb.append("<button onclick=\"hide_all();show_elements_perm(true, [");
	        	sb.append(path_buff.toString());
        		sb.append("])\" ");
			// setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">show it</button><br>\n");
		}
		sb.append("</UL>\n");
		return sb.toString();
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
            FileWriter fstream = new FileWriter(filename);
            BufferedWriter out = new BufferedWriter(fstream);
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
            FileWriter fstream = new FileWriter(htmlFileName);
            BufferedWriter out = new BufferedWriter(fstream);
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
        		List<HashMap<AttemptGoal, ConstraintPath>> l = unsatPaths.genIndependentPaths();
        		if (l.size()==1) {
        			out.append("<H2>One typing error is identified<H2>");
        		}
        		else {
        			out.append("<H2>"+l.size()+" separate typing errors are identified</H2>");
        		}
        		int count = 1;	
        		for (HashMap<AttemptGoal, ConstraintPath> paths : l)
        			out.append(getOneSuggestion(sourceName,paths, count++));
        		
        		out.append(genAnnotatedCode() +
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

    public void setShowHideActions(boolean isPath, StringBuffer sb, String loc, int id) {
		String num = isPath?"true":"false"; 
		sb.append(" onmouseover=\"show_elements("+num+", [");
		sb.append(loc+"]) ");
		if (!isPath)
			sb.append("; show_cut("+id+") ");
		sb.append("\"");
		sb.append(" onmouseout=\"hide_elements("+num+", [");
		sb.append(loc+"]) ");
		if (!isPath)
			sb.append("; hide_cut("+id+") ");
		sb.append("\"");
	}
    
    public String genMissingAssumptions (Map<AttemptGoal, ConstraintPath> errorPaths) {
    	StringBuffer sb = new StringBuffer();
    	if (GEN_ASSUMP) {
    		Set<Hypothesis> result = MissingHypoInfer.genAssumptions(errorPaths.keySet(), cachedEnv).iterator().next();
    		sb.append("<H3>Likely missing assumption(s): </H3>\n");
        	sb.append("<UL>\n");
        	sb.append("<LI>");
        	sb.append("<OL>\n");
        	
    		String missingCons = "";
    		for (Hypothesis g : result)
   				missingCons += g.toString()+";";
   			sb.append(missingCons+"\n");
        	sb.append("</OL>\n");
        	
   			if (pos!=null) {
   				sb.append("\n<pre class=\"code\">\n");
       	        try {
       	            FileReader fstream = new FileReader(sourceName);
       	            BufferedReader in = new BufferedReader(fstream);
       	            String current;
       	            int currentline=0;
       	            while ((current=in.readLine())!=null) {
       	            	currentline ++;
       	            	if (currentline < pos.getLineStart()) continue;
       	            	
       	            	if (pos.getLineStart()==currentline) {
       	            			current = current + 
       	            				" <span class=\"missingConstraint\"> " + missingCons + "</span>";
       	            	}
       	            	
       	            	sb.append(currentline + ". "+ current+"\n");
       	            }
       	            in.close();
       	        } catch (IOException e) {
       	            sb.append("Failed to read file: " + sourceName);
       	        }
       	    	sb.append("</pre>\n");
   			}
   			sb.append("</UL>");
    	}
    	return sb.toString();
    }
    
    private class ConsSuggestion implements Comparable<ConsSuggestion> {
		int rank = 0;
		Set<EquationEdge> edges;
		int id;

		public ConsSuggestion(int id, Set<EquationEdge> edges) {
			this.edges = edges;
			this.id = id;
			for (EquationEdge edge : edges) {
				rank += edge.getEquation().getSuccPaths();
			}
		}
    	
    	@Override
    	public int compareTo(ConsSuggestion o) {
    		return new Integer(rank).compareTo(o.rank);
    	}
    	
    	public String toHTML () {
    		StringBuffer sb = new StringBuffer();
//    		sb.append("<LI>\n");
    		sb.append("<span class=rank>(rank "+rank+")</span> ");
        	for (EquationEdge c : edges) {
        		StringBuffer locBuffer = new StringBuffer();
        		Element left = c.getEquation().getFirstElement();
        		locBuffer.append("['left', \'"+left.getPosition().toString()+"\'],");
        		Element right = c.getEquation().getSecondElement();
        		locBuffer.append("['right', \'"+right.getPosition().toString()+"\']");
        		String loc = locBuffer.toString();
        		sb.append("<span class=\"mincut\" ");
        		setShowHideActions(true, sb, loc, id);
        		sb.append(">");
        		sb.append("<code id=\"left"+id+"\">"+left.toDetailString()+"</code>");
        		sb.append(" has the same type as ");
        		sb.append("<code id=\"right"+id+"\">"+right.toDetailString()+"</code></span>");
        		sb.append("<button onclick=\"hide_all();show_elements_perm(false, [");
        		sb.append(loc);
        		sb.append("]); ");
        		sb.append("show_cut_perm("+id+")\" ");
        		// setShowHideActions(false, sb, loc, id);
        		sb.append(">show it</button><br>\n");
        	}
       		return sb.toString();
    	}
    }
    
    private class ExprSuggestion implements Comparable<ExprSuggestion> {
		int rank = 0;
		Set<String> exprs;
		int id;

		public ExprSuggestion(int id, Set<String> exprs) {
			this.exprs = exprs;
			this.id = id;
			for (String expr : exprs) {
				rank += succCount.get(expr);
			}
		}
    	
    	@Override
    	public int compareTo(ExprSuggestion o) {
    		return new Integer(rank).compareTo(o.rank);
    	}
    	
    	public String toHTML () {
    		StringBuffer sb = new StringBuffer();
//    		sb.append("<LI>\n");
    		sb.append("<span class=\"rank\">(rank "+rank+")</span> ");
			
			StringBuffer locBuffer = new StringBuffer();
        	StringBuffer exprBuffer = new StringBuffer();
			for (String c : exprs) {
				Element en = ((ElementNode)exprMap.get(c)).getElement();
        		locBuffer.append("['pathelement', \'"+en.getPosition()+"\'], ");
        		exprBuffer.append(en.toHTMLString()+"    ");
        	}
        	sb.append("<span class=\"path\" ");
			setShowHideActions(false, sb, locBuffer.toString(), 0);
			sb.append(">");
			sb.append("<code>"+exprBuffer.toString()+"</code></span>");
        	sb.append("<button onclick=\"hide_all();show_elements_perm(true, [");
	        sb.append(locBuffer.toString());
        	sb.append("])\" ");
			// setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">show it</button><br>\n");
        	
       		return sb.toString();
    	}
    }
    
    public String genCutItems (HashMap<AttemptGoal, ConstraintPath> errorPaths) {
    	StringBuffer sb = new StringBuffer();
    	
		if (GEN_CUT) {
			Set<Set<String>> results = null;
			long startTime = System.currentTimeMillis();
			results = unsatPaths.genNodeCuts(errorPaths.keySet());
			long endTime =  System.currentTimeMillis();
			System.out.println("top_rank_size: "+results.size());
			System.out.println("ranking_time: "+(endTime-startTime));
			
			sb.append("<H4>Expressions in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n");

			List<ExprSuggestion> cuts = new ArrayList<ExprSuggestion>();
			int count = 1;
			for (Set<String> set : results) {
				cuts.add(new ExprSuggestion(count, set));
				count++;
			}
			Collections.sort(cuts);

			int best=Integer.MAX_VALUE;
			int i=0;
			for ( ; i<cuts.size(); i++) {
				if (cuts.get(i).rank>best)
					break;
				best = cuts.get(i).rank;
				sb.append(cuts.get(i).toHTML());
			}
			sb.append("<button onclick=\"show_more_expr()\">show/hide more</button><br>\n");
			sb.append("<div id=\"more_expr\">");
			for ( ; i<cuts.size(); i++) {
				sb.append(cuts.get(i).toHTML());
			}
			sb.append("</div>\n");
		}
		
		if (GEN_CUT) {
			Set<Set<EquationEdge>> results = null;
			results = unsatPaths.genEdgeCuts(errorPaths.keySet());
			sb.append("<H4>Constraints in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n");

			// sb.append("<OL>\n");

			// get all cuts and rank them
			List<ConsSuggestion> cuts = new ArrayList<ConsSuggestion>();
			int count = 1;
			for (Set<EquationEdge> set : results) {
				cuts.add(new ConsSuggestion(count, set));
				count++;
			}
			Collections.sort(cuts);

                        int best=Integer.MAX_VALUE;
			int i=0;
			for ( ; i<cuts.size(); i++) {
				if (cuts.get(i).rank>best)
					break;
				best = cuts.get(i).rank;
				sb.append(cuts.get(i).toHTML());
			}
			sb.append("<button onclick=\"show_more_cons()\">show/hide more</button><br>\n");
			sb.append("<div id=\"more_cons\">");
			for ( ; i<cuts.size(); i++) {
				sb.append(cuts.get(i).toHTML());
			}
			sb.append("</div>\n");
			// sb.append("</OL>\n");
		}
		
    	return sb.toString();
    }
    
    // this class is used to ease the ordering of <line, column> pair
    private class LineColumnPair implements Comparable<LineColumnPair> {
    	int line;
    	int column;
    	// the following two field are only used to break ties
    	int endline;
    	int endcol;
    	String id; 
    	
    	public LineColumnPair(int line, int column, int endline, int endcol, String id) {
    		this.line = line;
    		this.column = column;
    		this.endline = endline;
    		this.endcol = endcol;
    		this.id = id;
		}
    	
    	public int compareTo(LineColumnPair p) {
    		if (line!=p.line)
    			return new Integer(line).compareTo(p.line);
    		if (column!=p.column)
    			return new Integer(column).compareTo(p.column);
    		// order is reverse
    		if (endline!=p.endline)
    			return new Integer(p.endline).compareTo(endline);
    		if (endcol!=p.endcol)
    			return new Integer(p.endcol).compareTo(endcol);
    		return 0;
    	}
    	    	
    	@Override
    	public int hashCode() {
    		return id.hashCode();
    	}
    	
    }
    
    // find all locations involved in the unsat paths, and then wrap the corresponding code with <span> </span> notations
    public String genAnnotatedCode () {
    	StringBuffer sb = new StringBuffer();
        sb.append("<button onclick=\"hide_all()\">hide all highlights</button><br>\n");
    	sb.append("\n<pre class=\"code\" id=\"code\">\n");
    	
    	// collect all position information, and sort them
    	List<LineColumnPair> startList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> endList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> emptyList = new ArrayList<LineColumnPair>(); // the set where start=end
    	Set<Position> posSet = new HashSet<Position>();
    	for (ConstraintPath path : unsatPaths.getPaths()) {
    		List<Node> nodes = path.getAllNodes();
    		for (Node node : nodes) {
    			posSet.add(((ElementNode)node).getElement().getPosition());
    		}
    		
    		List<Edge> edges = path.getEdges();
    		for (Edge edge : edges) {
    			if(edge instanceof EquationEdge)
    				posSet.add(((EquationEdge)edge).getEquation().getPos());
    		}
    	}
    	
    	for (Position pos : posSet) {
			if (!pos.isEmpty()) {
				if (pos.getLineStart()==pos.getLineEnd() && pos.getColStart() == pos.getColEnd())
					emptyList.add(new LineColumnPair(pos.getLineStart(), pos.getColStart(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
				else {
					startList.add(new LineColumnPair(pos.getLineStart(), pos.getColStart(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
					endList.add(new LineColumnPair(pos.getLineEnd(), pos.getColEnd(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
				}
			}
    	}
    	
    	Collections.sort(startList);
    	Collections.sort(endList);
    	Collections.sort(emptyList);
    	
    	int startIndex = 0;
    	int endIndex = 0;
    	int emptyIndex = 0;
		LineColumnPair start = startList.get(startIndex++);
		LineColumnPair end = endList.get(endIndex++);
		LineColumnPair empty;
		if (emptyList.isEmpty())
			empty = new LineColumnPair(-1, -1, -1, -1, "");
		else
			empty = emptyList.get(emptyIndex++);
		
    	// add annotations to the source
    	try {
			FileReader fstream = new FileReader(sourceName);
			BufferedReader in = new BufferedReader(fstream);
			String current;
			int currentline = 0;
			while ((current = in.readLine()) != null) {
				currentline++;
				sb.append("<span class=lineno>" + currentline);
				sb.append(".</span>");

				if (end.line != currentline && start.line != currentline && empty.line != currentline) {
					sb.append(current);
					sb.append("\n");
					continue;
				}
				
				int col = 0;
				char[] chars = current.toCharArray();
				for ( ; col<chars.length; col++) {
					// handle end annotation first
					while (end.line==currentline && end.column==col) {
						sb.append("</span>");
						if (endIndex<endList.size())
							end = endList.get(endIndex++);
						else
							break;
					}
					// handle the annotations where start = end
					while (empty.line==currentline && empty.column==col) {
						sb.append("<span class=\"moreinfor\" id=\""+empty.id+"\">");
						sb.append("</span>");
						if (emptyIndex<emptyList.size())
							empty = emptyList.get(emptyIndex++);
						else
							break;
					}
					while (start.line==currentline && start.column==col) {
						sb.append("<span class=\"moreinfor\" id=\""+start.id+"\">");
						if (startIndex<startList.size())
							start = startList.get(startIndex++);
						else
							break;
					}
					sb.append(chars[col]);
				}
				// handle the possible end annotation after the last character
				while (end.line==currentline && end.column>=col) {
					sb.append("</span>");
					if (endIndex<endList.size())
						end = endList.get(endIndex++);
					else
						break;
				}
				sb.append("\n");
			}
			in.close();
		} catch (IOException e) {
			sb.append("Failed to read file: " + sourceName);
		}
		sb.append("</pre>\n");
		return sb.toString();
	}
}
