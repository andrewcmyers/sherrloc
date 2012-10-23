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
import java.util.Set;
import java.util.Stack;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import util.AttemptGoal;
import constraint.ast.Constraint;
import constraint.ast.ConstructorElement;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
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
	HashMap<AttemptGoal, ConstraintPath> errorPaths;
	HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	HashSet<AttemptGoal> unsatPath;						// source and sink of the unsatisfiable paths. This set is filled by function genErrorPaths, and used by genAssumptions
	String sourceName;
	String htmlFileName;
	Position pos=null;
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        errorPaths = new HashMap<AttemptGoal, ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
        unsatPath = new HashSet<AttemptGoal>();
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
				Analysis ana = Analysis.getAnalysisInstance(diagfile, symmentric);// "src/constraint/tests/jif/AirlineAgent.con",
									// symmentric);
				ana.SHOW_WHOLE_GRAPH = whole_graph;
				ana.GEN_ASSUMP = assumption;
				ana.GEN_CUT = cut;
			    ana.sourceName = infile;
				ana.htmlFileName = outfile;
				ana.writeToDotFile();
				ana.writeToHTML();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	static public Analysis getAnalysisInstance (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;
//	    for (Equation e: result) {
//	    	System.out.println( e.toString());
//	    }
	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    Analysis ret = new Analysis(graph);
	    // read the position of method generating the constraints when available
		BufferedReader br = new BufferedReader(new FileReader(input));
		String firstline = br.readLine();
//		if (firstline.contains("jif")) {
//			ret.pos = new Position(firstline.substring(firstline.indexOf("jif")));
//		}
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
        
        // only the labels without variables can serve as end nodes
        ArrayList<ElementNode> startNodes = new ArrayList<ElementNode>();
        ArrayList<ElementNode> endNodes = new ArrayList<ElementNode>();
        Set<Element> elements = graph.getAllElements();
        
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
				
		for (ElementNode start : startNodes) {
			for (ElementNode end : endNodes) {
				Element e1 = start.getElement();
				Element e2 = end.getElement();
				List<Edge> l = finder.getPath(start, end);
				
				if (graph.isSymmentric() && (graph.getIndex(start) < graph.getIndex(end)))
					continue;
				
				if (l==null) continue;
				
				ConstraintPath path = new ConstraintPath(l, finder);

				if (e1 instanceof ConstructorElement && e2 instanceof ConstructorElement) {
					if (!((ConstructorElement)e1).getCons().sameas(((ConstructorElement)e2).getCons())) {
//						System.out.println(path.toString());
						path.setCause();
						AttemptGoal goal = new AttemptGoal(start, end, path.getAssumption());
						unsatPath.add(goal);
						errorPaths.put(goal, path);
						continue;
					}
					else {
						path.incSuccCounter();
						continue;
					}
				}

				if ( e1 instanceof Variable || e2 instanceof Variable)
					continue;
				
				// successful path
				if (graph.getEnv().leq(start.getElement(), end.getElement())) {
					path.incSuccCounter();
					continue;
				}

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
				if (env.leq(start.getElement(), end.getElement())) {
					path.incSuccCounter();
					continue;
				}
				path.setCause();
				// System.out.println(path.toString());
				AttemptGoal goal = new AttemptGoal(start, end, env);
				unsatPath.add(goal);
				errorPaths.put(goal, path);
			}
		}
		done = true;

	}
    
    public String unsatPathsToHTML () {
    	StringBuffer sb = new StringBuffer();
    	sb.append("<H3>");
    	sb.append(errorPaths.size() +" type error(s) found: \n");
		sb.append("</H3>\n");
		sb.append("<UL>\n");
		for (AttemptGoal goal : unsatPath) {
			//sb.append("<div class=\"moreinfo\"> " +
			//		errorPaths.get(goal).toString() + " \n");
			//sb.append("</div>);
			StringBuffer path_buff = new StringBuffer();
			List<Node> nodes = errorPaths.get(goal).getNodes();
			for (Node n : nodes) {
				path_buff.append("'"+((ElementNode)n).getElement().getPosition()+"', ");
			}
			sb.append("<LI>\n<span class=\"path\" onmouseover=\"show_elements('pathelement', [");
			sb.append(path_buff.toString());
			sb.append("])\"");
			sb.append(" onmouseout=\"hide_elements([");
			sb.append(path_buff.toString());
			sb.append("]) \">");
			sb.append("A value with type "+goal.getSource().getElement().toHTMLString() + 
				    " is being used at type " + goal.getSink().getElement().toHTMLString());
			sb.append("</span>\n");
        	sb.append("<button onclick=\"hide_elements_perm();show_elements_perm('pathelement', [");
        	sb.append(path_buff.toString());
        	sb.append("])\" ");
			setShowHideActions(sb, "pathelement", path_buff.toString());
			sb.append(">Show it</button><br>\n");
		}
		sb.append("</UL>\n");
		return sb.toString();
    }
    
    public Set<AttemptGoal> genAssumptions (Set<AttemptGoal> remaining) {    	    	
    	HashMap<AttemptGoal, List<AttemptGoal>> dep = genAssumptionDep(remaining);
    	Set<Set<AttemptGoal>> results = new HashSet<Set<AttemptGoal>>();
    	
    	// we do an interative deeping search until at least one cut is returned
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, remaining, dep, new HashSet<AttemptGoal>(), results);
   			if (results.size()!=0)
   				break;
    	}

//		System.out.println("Likely missing assumptions:");
		HashSet<AttemptGoal> ret = new HashSet<AttemptGoal>();
    	for (Set<AttemptGoal> result : results) {
    		for (AttemptGoal s : result) {
    			ret.add(s);
//        		System.out.print(" "+s.getSource().getElement() +" <= "+s.getSink().getElement()+";");
    		}
//    		System.out.println();
    	}
    	
    	return ret;
    }
    
    /* Calculating a min cut is NP complete. Currently, we use interative deeping search to quickly identify the goal */
    public Set<Set<EquationEdge>> genCuts (Set<AttemptGoal> remaining) {
    	HashSet<EquationEdge> candidates = new HashSet<EquationEdge>();
    	Set<Set<EquationEdge>> results = new HashSet<Set<EquationEdge>>();
    	HashMap<AttemptGoal, List<EquationEdge>> map = new HashMap<AttemptGoal, List<EquationEdge>>();

    	for (AttemptGoal goal : remaining) {
    		List<EquationEdge> l = new ArrayList<EquationEdge>();
    		for (Edge e : errorPaths.get(goal).getEdges()) {
    			if (e instanceof EquationEdge) {
    				l.add((EquationEdge)e);
    				candidates.add((EquationEdge)e);
    			}
    		}
    		map.put(goal, l);
    	}
    	
    	// we do an interative deeping search until at least one cut is returned
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, candidates, map, new HashSet<EquationEdge>(), results);
   			if (results.size()!=0)
   				break;
    	}

    	return results;
    }
    
    public <K> void boundedDepthSearch (int level, Set<K> candidates, HashMap<AttemptGoal, List<K>> dependencies, Set<K> visited, Set<Set<K>> results) {
    	
    	/* first level */
   		for (K e : candidates) {
   			if (visited.contains(e))
   				continue;
   			else
   				visited.add(e);
   			
   			if (level == 1) {
   				boolean iscut = true;
   				
   				// for any path, at least one element in the visited list should appear
   	   			for (AttemptGoal goal : dependencies.keySet()) {
   	   				boolean flag = false;
   	   				for (K edge : visited) {
   	   					if (dependencies.get(goal).contains(edge)) {
   	   						flag = true;
   	   						break;
   	   					}
   	   				}
   	   				if (!flag) {
   	   					iscut = false;
   	 	   	   			break;
   	   				}
   	    		}
   	   			
   	   			if (iscut) {
   	   				HashSet<K> s = new HashSet<K>();
   	   				for (K eedge : visited) 
   	   					s.add(eedge);
   	   				results.add(s);
   	   			}
   			}
   			else
   				boundedDepthSearch (level-1, candidates, dependencies, visited, results);
   			
   			visited.remove(e);
   		}
   		    	
//    		ElementNode src = tri.getFirst();
//			ElementNode snk = tri.getSecond();
//			Environment env = tri.getThird();
//			
//			Set<Assumption> result = getAssumptions(src, snk, env);
//
//			System.out.println("**********************");
//	    	System.out.println("To make "+src.getElement() +" <= "+snk.getElement()+", we need ANY of the following");
//	    	for (Assumption p : result) {
//	    		System.out.println(p);
//	    	}
//	    	System.out.println("**********************");
//
//			conjunctSets.add(result);
//			break;
//    	}
//    	
//    	Stack<Assumption> s = new Stack<Assumption>();
//    	Set<AssumptionSet> result = new HashSet<AssumptionSet>();
//    	regGenAssumptions(0, conjunctSets, s, result);
//    	
//        AssumptionSet[] all = result.toArray(new AssumptionSet[result.size()]);
//        Arrays.sort(all);
//        
//        System.out.println("\n"+"Ranking of missing assumptions:");
//        for (AssumptionSet a : all) {
//            System.out.println(a.getSize() + ": "+a);
//        }
    }
    
    // this function returns hashmap, that for each goal, a list of "stronger" assumptions that either of them eliminates it
    public HashMap<AttemptGoal, List<AttemptGoal>> genAssumptionDep (Set<AttemptGoal> paths) {
    	HashMap<AttemptGoal, List<AttemptGoal>> ret = new HashMap<AttemptGoal, List<AttemptGoal>>( );
    	for (AttemptGoal goal : paths) {
    		List<AttemptGoal> list = new ArrayList<AttemptGoal>();
    		list.add(goal);
    		ret.put(goal, list);	// the goal itself is the weakest assumption
    		
    		for (AttemptGoal candidate : paths) {
    			if (candidate.equals(goal)) continue;

    			Environment env = goal.getEnv().addLeq(candidate.getSource().getElement(), candidate.getSink().getElement());
    			
				if (cachedEnv.containsKey(env))
					env = cachedEnv.get(env);
				else {
					cachedEnv.put(env, env);
				}
    			
    			if (env.leq( goal.getSource().getElement(), goal.getSink().getElement()))
    				list.add(candidate);
    		}
    	}
    	return ret;
    }
    
    public void regGenAssumptions (int index, List<Set<Assumption>> l, Stack<Assumption> s, Set<AssumptionSet> result) {
    	
    	for (Assumption p : l.get(index)) {
			s.add(p);
			if (index!=(l.size()-1))
				regGenAssumptions (index+1, l, s, result);
			else {
				AssumptionSet a = new AssumptionSet(s);
				result.add(a);
			}
			s.pop();
		}
    }
    
    /*
     * This function returns a set of pairs s.t. if ANY of them is satisfied, then e1<=e2
     */
    public Set<Assumption> getAssumptions(ElementNode e1, ElementNode e2, Environment env) {
    	Set<Assumption> ret = new HashSet<Assumption>();
    	
    	if (e1.getElement() instanceof MeetElement) {
    		for (Element e : ((MeetElement) e1.getElement()).getElements()) {
				ret.addAll(getAssumptions(graph.getNode(e), e2, env));
			}
    	}
    	else if (e2.getElement() instanceof JoinElement) {
    		for (Element e : ((JoinElement) e2.getElement()).getElements()) {
    			ret.addAll(getAssumptions(e1, graph.getNode(e), env));
    		}
    	}
    	else {
    		Set<Node> sourceSet = env.geqSet(e1);
    		Set<Node> sinkSet = env.leqSet(e2);
    		for (Node n1 : sourceSet) {
    			for (Node n2 : sinkSet) {
    				if (!n1.equals(n2)) {
    					ElementNode en1 = (ElementNode) n1;
    					ElementNode en2 = (ElementNode) n2;
    			    	if (en1.getElement() instanceof ConstructorElement && en2.getElement() instanceof ConstructorElement ) {
    						EnumerableElement ele1 = (EnumerableElement) en1.getElement();
    						EnumerableElement ele2 = (EnumerableElement) en2.getElement();
    			        	List<Element> compset1 = ele1.getElements();
    			        	List<Element> compset2 = ele2.getElements();
    			        	
    			            for (int index=0; index<compset1.size(); index++) {
    							ret.add( new Assumption(graph.getNode(compset1.get(index)), graph.getNode(compset2.get(index))));
    			            }
    					}
    			    	else
    			    		ret.add(new Assumption((ElementNode)n1, (ElementNode)n2));
    				}
    			}
    		}
    	}
    	return ret;
    }
    
    public int getPathNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	int ret = errorPaths.size();
    	printRank();
    	return ret;
    }
    
    public int getAssumptionNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
        Set<AttemptGoal> result = genAssumptions(errorPaths.keySet());
    	return result.size();
    }
    
    void printRank () {    	
        Constraint[] all = graph.getConstraints().toArray(new Constraint[graph.getConstraints().size()]);
        Arrays.sort(all); 
        for (Constraint equ : all) {
            if (equ.getRank() >0) 
                System.out.println(equ.getRank() + ": " + equ.toString());
        }
    }
    
    public void showErrorPaths() {
    	for (ConstraintPath path : errorPaths.values())
    		System.out.println(path.toString());
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
        
        if (GEN_ASSUMP)
        	genAssumptions(errorPaths.keySet());
        	
        try {
            FileWriter fstream = new FileWriter(htmlFileName);
            BufferedWriter out = new BufferedWriter(fstream);
            // out.write(getHeader());
            writeFeedback(out);
            out.write(getOneSuggestion(sourceName));
            // out.write(getTail());
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
    			"Error Diagnostic Report\n" +
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
    	out.write("<div id=feedback class=feedback action=\"\">\r\n");
    	out.write("<form>");
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
	out.write("<span id=\"hide_button\" onclick=\"hide_feedback_form()\">hide</span>\r\n");
	
    	out.write("</form></div>\r\n");
    }
    
    public String getTail () {
    	return 	"\n\n" +
    			"</BODY>\n" +
    			"</HTML>";
    }

    public void setShowHideActions(StringBuffer sb, String classname, String loc) {
		sb.append(" onmouseover=\"show_elements('"+classname+"', [");
		sb.append(loc);
		sb.append("]) \" ");
		sb.append(" onmouseout=\"hide_elements([");
		sb.append(loc);
		sb.append("]) \" ");
	}
    
    public String getOneSuggestion (String sourcefile) {
    	StringBuffer sb = new StringBuffer();
    	sb.append( "<!-- ======== START OF ERROR REPORT ======== -->\n" +
    			"<H2>\n" +
    			"<BR>\n" +
    			"Error Diagnostic Report </H2>\n" +
    			"<HR>\n");
    	// type check succeeded
    	if (unsatPath.size()==0) {
    		sb.append("<H3>");
			sb.append("The program passed type checking. No errors were found.");
            sb.append("<script type=\"text/javascript\">"+
                      "document.getElementById('feedback').style.display = 'none';</script>");
    	}
    	else {
    		sb.append(
    			unsatPathsToHTML() +
    			"<HR>\n" +
    			"<H3>\n" +
    			"Suggestions</H3>\n" +
    			genMissingAssumptions() +
    			genCutItems() +
    			genAnnotatedCode() +
    			(sourceName.contains("jif")?("<script>display_info('info'); colorize_all(); numberSuggestions();</script>\n")
    					:("<script>display_info('info'); numberSuggestions();</script>\n")));
    	}
    	return sb.toString();
    }
    
    public String genMissingAssumptions () {
    	StringBuffer sb = new StringBuffer();
    	if (GEN_ASSUMP) {
    		Set<AttemptGoal> result = genAssumptions(errorPaths.keySet());
    		sb.append("Likely missing assumption(s): \n");
        	sb.append("<UL>\n");
        	sb.append("<LI>");
        	sb.append("<OL>\n");
        	
    		String missingCons = "";
    		for (AttemptGoal g : result)
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
    
    private class CutSuggestion implements Comparable<CutSuggestion> {
    	int rank=0;
    	Set<EquationEdge> edges;
    	
    	public CutSuggestion(Set<EquationEdge> edges) {
    		this.edges = edges;
    		for (EquationEdge edge : edges) {
    			rank += edge.getEquation().getSuccPaths();
    		}
		}
    	
    	@Override
    	public int compareTo(CutSuggestion o) {
    		return new Integer(rank).compareTo(o.rank);
    	}
    	
    	public String toHTML () {
    		StringBuffer sb = new StringBuffer();
//    		sb.append("<LI>\n");
    		StringBuffer locBuffer = new StringBuffer();
    		StringBuffer textBuffer = new StringBuffer();
        	for (EquationEdge c : edges) {
        		locBuffer.append("'"+c.getEquation().getFirstElement().getPosition()+"',");
        		locBuffer.append("'"+c.getEquation().getSecondElement().getPosition()+"',");
   				textBuffer.append("<code>"+c.getEquation().toHTMLString() +"</code> ; ");
        	}

			String loc = locBuffer.toString();
			sb.append("<span class=\"mincut\" ");
			setShowHideActions(sb, "cut", loc);
			sb.append(">");

    		sb.append(textBuffer.toString());
    		sb.append("(weight="+rank+")");
			sb.append("</span>\n");
        	sb.append("<button onclick=\"hide_elements_perm();show_elements_perm('cut', [");
        	sb.append(loc);
        	sb.append("])\" ");
			setShowHideActions(sb, "cut", loc);
			sb.append(">Show it</button><br>\n");
			return sb.toString();
    	}
    }
    
    public String genCutItems () {
    	StringBuffer sb = new StringBuffer();
    	
    	if (GEN_CUT) {
            Set<Set<EquationEdge>> results=null;
        	results = genCuts(errorPaths.keySet());
        	sb.append("Constraints in the source code that appear most likely to be wrong (mouse over to highlight code):<br>");

//        	sb.append("<OL>\n");
        	
        	// get all cuts and rank them
        	List<CutSuggestion> cuts = new ArrayList<CutSuggestion>();
        	for (Set<EquationEdge> set : results) 
        		cuts.add(new CutSuggestion(set));
        	Collections.sort(cuts);
        	
        	for (CutSuggestion cut : cuts) {
        		sb.append(cut.toHTML());
       		}
//   			sb.append("</OL>\n");
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
    	sb.append("\n<pre class=\"code\" id=\"code\">\n");
    	
    	// collect all position information, and sort them
    	List<LineColumnPair> startList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> endList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> emptyList = new ArrayList<LineColumnPair>(); // the set where start=end
    	Set<Position> posSet = new HashSet<Position>();
    	for (ConstraintPath path : errorPaths.values()) {
    		List<Node> nodes = path.getNodes();
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
				currentline ++;
				sb.append(currentline + ". ");

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
				while (end.line==currentline && end.column==col) {
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
