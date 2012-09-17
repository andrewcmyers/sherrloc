package diagnostic;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
		options.addOption("s", false, "symmentric");
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
		String outfile = "error.con";
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
			outfile = cmd.getOptionValue("o")+".con";
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
		if (firstline.contains("jif")) {
			ret.pos = new Position(firstline.substring(firstline.indexOf("jif")));
		}
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
        
        // only the labels without varables can serve as end nodes
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
				if (l==null) continue;
				
				if (e1 instanceof ConstructorElement && e2 instanceof ConstructorElement) {
					if (!((ConstructorElement)e1).getCons().equals(((ConstructorElement)e2).getCons()) &&
							(!graph.isSymmentric() || (graph.getIndex(start) < graph.getIndex(end)))) {
						ConstraintPath path = new ConstraintPath(l);
//						System.out.println(path.toString());
						path.setCause();
						AttemptGoal goal = new AttemptGoal(start, end, path.getAssumption());
						unsatPath.add(goal);
						errorPaths.put(goal, path);
						continue;
					}
					
					if (e1.hasVars() || e2.hasVars())
						continue;
				}
				
				if (start.getElement() instanceof ConstructorElement && end.getElement() instanceof ConstructorElement)
					continue;

				if (graph.getEnv().leq(start.getElement(), end.getElement()))
					continue;

				if (!graph.isSymmentric() || (graph.getIndex(start) < graph.getIndex(end))) {

					ConstraintPath path = new ConstraintPath(l);
					Environment env;
					if (cachedEnv.containsKey(path.getAssumption()))
						env = cachedEnv.get(path.getAssumption());
					else {
						env = new Environment();
						env.addEnv(graph.getEnv());
						env.addEnv(path.getAssumption());
						cachedEnv.put(path.getAssumption(), env);
					}
										
					if (env.leq(start.getElement(), end.getElement()))
						continue;
					path.setCause();
//					System.out.println(path.toString());
					AttemptGoal goal = new AttemptGoal(start, end, env);
					unsatPath.add(goal);
					errorPaths.put(goal, path);
				}
			}
		}
		done = true;

	}
    
    public String unsatPathsToHTML () {
    	StringBuffer sb = new StringBuffer();
    	sb.append("<span id=\"info\" class=\"slice\"> Slice of " + errorPaths.size() +" unsat path(s): \n");
		sb.append("<pre>\n");
		for (AttemptGoal goal : unsatPath) {
			//sb.append("<div class=\"moreinfo\"> " +
			//		errorPaths.get(goal).toString() + " \n");
			//sb.append("</div>);
			sb.append("Unsat path from "+goal.getSource().getElement() + " to " + goal.getSink().getElement()+"\n");
		}
		sb.append("</pre>\n");
		sb.append("</span>");
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
    	String filename;

        filename = "error.html";
        
        if (!done) 
        	genErrorPaths();
        
        if (GEN_ASSUMP)
        	genAssumptions(errorPaths.keySet());
        	
        StringBuffer sb = new StringBuffer();
        sb.append(getHeader());
    	   		
        try {
            FileWriter fstream = new FileWriter(filename);
            BufferedWriter out = new BufferedWriter(fstream);
            out.write(getHeader());
            out.write(getOneSuggestion(sourceName));
            out.write(getTail());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the HTML file to: " + filename);
        }
    }
    
    public String getHeader () {
    	return "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n" +
    			"<!--NewPage-->\n" +
    			"<HTML>\n" +
    			"<HEAD>\n" +
    			"<!-- Generated by diagnostic tool on Sun Aug 15 14:49:41 BST 2010 -->\n" +
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
    			"<script type=\"text/javascript\">\n" +
    			"function display_info(id) {\n" +
    			"\tvar elem = document.getElementById(id);\n" +
    			"\telem.style.display = 'block';\n" +
    			"}\n" +
    			"function hide(id) {\n" +
    			"\tvar elem = document.getElementById(id);\n" +
    			"\telem.style.display = 'none';\n" +
    			"}\n" +
    			"</script>\n" +
    			
    			"\n</HEAD>\n" +
    			
    			"\n<BODY BGCOLOR=\"white\" onload=\"windowTitle();\">\n" +
    			"<HR>\n";
    }
    
    public String getTail () {
    	return 	"<HR>\n\n" +
    			"</BODY>\n" +
    			"</HTML>";
    }
    
    public String getOneSuggestion (String sourcefile) {
    	StringBuffer sb = new StringBuffer();
    	sb.append( "<!-- ======== START OF ERROR REPORT ======== -->\n" +
    			"<H2>\n" +
    			"<BR>\n" +
    			"Error Diagnostic Report for file " + sourceName + " </H2>\n" +
    			unsatPathsToHTML() + 
    			"<HR>\n" +
    			"<H3>\n" +
    			"<BR>\n" +
    			"Most likely suggestions</H3>\n" +
    			"<UL>\n" +
    			genMissinAssumptions() +
    			genCutItems() +
    			"<script>display_info('info')</script>\n"+
    			"</UL>\n");
    	
    	return sb.toString();
    }
    
    public String genMissinAssumptions () {
    	StringBuffer sb = new StringBuffer();
    	if (GEN_ASSUMP) {
    		Set<AttemptGoal> result = genAssumptions(errorPaths.keySet());
    		sb.append("<LI> Likely missing assumption(s): \n");
   			
    		String missingCons = "";
    		for (AttemptGoal g : result)
   				missingCons += g.toString()+";";
   			sb.append(missingCons+"\n");
   			
   			if (pos!=null) {
   				sb.append("\n<pre class=\"code\">\n");
       	        try {
       	            FileReader fstream = new FileReader(sourceName);
       	            BufferedReader in = new BufferedReader(fstream);
       	            String current;
       	            int currentline=0;
       	            while ((current=in.readLine())!=null) {
       	            	currentline ++;
       	            	if (currentline < pos.getLine()) continue;
       	            	
       	            	if (pos.getLine()==currentline) {
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
    	}
    	return sb.toString();
    }
    
    public String genCutItems () {
    	StringBuffer sb = new StringBuffer();
    	
    	if (GEN_CUT) {
            Set<Set<EquationEdge>> results=null;
        	results = genCuts(errorPaths.keySet());
        	sb.append("<LI> Likely wrong constraints in the source code:\n");
        	sb.append("<UL>\n");
       		for (Set<EquationEdge> set : results) {
            	sb.append("<LI>\n");
            	
       			for (EquationEdge c : set)
       				sb.append(c.getEquation()+"\n");
       			
       	    	sb.append("\n<pre class=\"code\">\n");
       	        try {
       	            FileReader fstream = new FileReader(sourceName);
       	            BufferedReader in = new BufferedReader(fstream);
       	            String current;
       	            int currentline=1;
       	            while ((current=in.readLine())!=null) {
       	            	
       	            	for (EquationEdge c : set) {
       	            		Position p = new Position(c.getEquation().getInfo());
       	            		if (p.getLine()==currentline) {
       	            			current = current.substring(0, p.getColStart()) + 
       	            				"<span id=\"wrongcons\" class=\"moreinfo\"> "+
       	            				c.getEquation() +
       	            				"</span>" + 	           
       	            				"<span class=\"wrongConstraint\" onmouseover=\"display_info('wrongcons')\" onmouseout=\"hide('wrongcons')\">" + 
       	            				current.substring(p.getColStart(), p.getColEnd()) + "</span>" + current.substring(p.getColEnd());
       	            		}
       	            	}
       	            	sb.append(current+"\n");
       	            	currentline ++;
       	            }
       	            in.close();
       	        } catch (IOException e) {
       	            sb.append("Failed to read file: " + sourceName);
       	        }
       	    	sb.append("</pre>\n");
       		}
       		sb.append("</UL>\n");
        }
    	
    	return sb.toString();
    }
}
