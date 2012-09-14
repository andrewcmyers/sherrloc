package diagnositc;

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

import util.AttemptGoal;
import constraint.ast.Constraint;
import constraint.ast.ConstructorElement;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
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
	boolean DEBUG = true;
    boolean SHOW_WHOLE_GRAPH=false;
    int REC_MAX = 5;
	boolean done = false;
	ConstraintGraph graph;
	HashMap<AttemptGoal, ConstraintPath> errorPaths;
	HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	HashSet<AttemptGoal> unsatPath;						// source and sink of the unsatisfiable paths. This set is filled by function genErrorPaths, and used by genAssumptions
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        errorPaths = new HashMap<AttemptGoal, ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
        unsatPath = new HashSet<AttemptGoal>();
	}
	
	public static void main(String[] args) {
		try {
			Analysis ana = Analysis.getAnalysisInstance("src/constraint/tests/jif/r3142.con", false);
//			Analysis ana = Analysis.getAnalysisInstance("src/constraint/tests/sml/test2.con", true);
			ana.writeToDotFile();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	static public Analysis getAnalysisInstance (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;
//	    for (Equation e: result) {
//	    	System.out.println( e.toString());
//	    }
	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    return new Analysis(graph);
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
						System.out.println(path.toString());
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
					System.out.println(path.toString());
					AttemptGoal goal = new AttemptGoal(start, end, env);
					unsatPath.add(goal);
					errorPaths.put(goal, path);
				}
			}
		}
		done = true;
		
		if (DEBUG)
			System.out.println("*** Found "+errorPaths.size() + " in total");
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

		System.out.println("Possible missing assumptions:");
		HashSet<AttemptGoal> ret = new HashSet<AttemptGoal>();
    	for (Set<AttemptGoal> result : results) {
    		for (AttemptGoal s : result) {
    			ret.add(s);
        		System.out.print(" "+s.getSource().getElement() +" <= "+s.getSink().getElement()+";");
    		}
    		System.out.println();
    	}
    	
    	return ret;
    }
    
    /* Calculating a min cut is NP complete. Currently, we use interative deeping search to quickly identify the goal */
    public void genCuts (Set<AttemptGoal> remaining) {
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
    	
   		for (Set<EquationEdge> set : results) {
   			System.out.println("********** cut *****");
   			for (EquationEdge c : set) {
   				System.out.println (c.getEquation());
   			}
   			System.out.println("********************");
   		}

    	
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
    			if (goal.getEnv().addLeq(candidate.getSource().getElement(), candidate.getSink().getElement()).leq( goal.getSource().getElement(), goal.getSink().getElement()))
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
        
        Set<AttemptGoal> missingAssump =  genAssumptions(errorPaths.keySet());
//        genCuts(errorPaths.keySet());
//        Set<AttemptGoal> remainingAssump = new HashSet<AttemptGoal>();
//        for (AttemptGoal goal : errorPaths.keySet()) {
//        	if (!missingAssump.contains(goal))
//        		remainingAssump.add(goal);
//        }
//        genAssumptions(remainingAssump);
        
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
        
        printRank();
    }
}
