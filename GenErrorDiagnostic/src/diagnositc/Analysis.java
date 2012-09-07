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

import util.Triple;
import constraint.ast.Constraint;
import constraint.ast.ConstructorElement;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.graph.ConstraintGraph;
import constraint.graph.ConstraintPath;
import constraint.graph.ConstructorEdge;
import constraint.graph.Edge;
import constraint.graph.EdgeCondition;
import constraint.graph.ElementNode;
import constraint.graph.Graph;
import constraint.graph.JoinEdge;
import constraint.graph.MeetEdge;
import constraint.graph.Node;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
	boolean DEBUG = true;
    boolean SHOW_WHOLE_GRAPH=false;
	boolean done = false;
	ConstraintGraph graph;
	List<ConstraintPath> errorPaths;
	HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	HashSet<Triple<ElementNode, ElementNode, Environment>> unsatPath;						// source and sink of the unsatisfiable paths. This set is filled by function genErrorPaths, and used by genAssumptions
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        errorPaths = new ArrayList<ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
        unsatPath = new HashSet<Triple<ElementNode,ElementNode, Environment>>();
	}
	
	public static void main(String[] args) {
		try {
			Analysis ana = Analysis.getAnalysisInstance("src/constraint/tests/jif/r3122.con", false);
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
				if (start.getElement().isDecomposable() && end.getElement().isDecomposable())
					continue;

				if (graph.getEnv().leq(start.getElement(), end.getElement()))
					continue;

				List<Edge> l = finder.getPath(start, end);
				if ( l!=null && (!graph.isSymmentric() || (graph.getIndex(start) < graph.getIndex(end)))) {

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
					unsatPath.add(new Triple<ElementNode, ElementNode, Environment>(start, end, env));
					errorPaths.add(path);
				}
			}
		}
		done = true;
		
		if (DEBUG)
			System.out.println("*** Found "+errorPaths.size() + " in total");
	}
    
    public void genAssumptions () {    	
    	List<Set<Assumption>> conjunctSets = new ArrayList<Set<Assumption>>();
    	
    	for (Triple<ElementNode, ElementNode, Environment> tri : unsatPath) {
    		ElementNode src = tri.getFirst();
			ElementNode snk = tri.getSecond();
			Environment env = tri.getThird();
			
			conjunctSets.add(getAssumptions(src, snk, env));
			break;
    	}
    	
    	Stack<Assumption> s = new Stack<Assumption>();
    	Set<AssumptionSet> result = new HashSet<AssumptionSet>();
    	regGenAssumptions(0, conjunctSets, s, result);
    	
        AssumptionSet[] all = result.toArray(new AssumptionSet[result.size()]);
        Arrays.sort(all);
        
        System.out.println("\n"+"Ranking of missing assumptions:");
        for (AssumptionSet a : all) {
            System.out.println(a.getSize() + ": "+a);
        }
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
    	else if (e1.getElement() instanceof ConstructorElement && e2.getElement() instanceof ConstructorElement ) {
			EnumerableElement ce1 = (EnumerableElement) e1.getElement();
			EnumerableElement ce2 = (EnumerableElement) e2.getElement();
        	List<Element> compset1 = ce1.getElements();
        	List<Element> compset2 = ce2.getElements();
        	
            for (int index=0; index<compset1.size(); index++) {
				getAssumptions(graph.getNode(compset1.get(index)), graph.getNode(compset2.get(index)), env);
            }
		}
    	else {
    		Set<Node> sourceSet = env.geqSet(e1);
    		Set<Node> sinkSet = env.leqSet(e2);
    		for (Node n1 : sourceSet) {
    			for (Node n2 : sinkSet) {
    				if (!n1.equals(n2)) {
    					ret.add(new Assumption((ElementNode)n1, (ElementNode)n2));
    				}
    			}
    		}
    	}
    	System.out.println("**********************");
    	System.out.println("To make "+e1.getElement() +" <= "+e2.getElement()+", we need ANY of the following");
    	for (Assumption p : ret) {
    		System.out.println(p);
    	}
    	System.out.println("**********************");
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
    
    void printRank () {    	
        Constraint[] all = graph.getConstraints().toArray(new Constraint[graph.getConstraints().size()]);
        Arrays.sort(all); 
        for (Constraint equ : all) {
            if (equ.getRank() >0) 
                System.out.println(equ.getRank() + ": " + equ.toString());
        }
    }
    
    public void showErrorPaths() {
    	for (ConstraintPath path : errorPaths)
    		System.out.println(path.toString());
    }
    
    public void writeToDotFile () {
        String filename;

        filename = "error.dot";
        
        if (!done) 
        	genErrorPaths();
        
        genAssumptions();
        
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
