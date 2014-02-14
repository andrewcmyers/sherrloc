package constraint.analysis;

import graph.ConstraintGraph;
import graph.ConstraintPath;
import graph.Edge;
import graph.Node;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.Hypothesis;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import diagnostic.UnsatPaths;

/**
 * This class identifies satisfiable and unsatisfiable constraints in a
 * constraint graph
 */
public class ConstraintAnalysis {
	UnsatPaths unsatPaths;
	ConstraintGraph graph;
	boolean isSym;
	boolean isVerbose;
	boolean isRec;
	boolean DEBUG = false;
	boolean done = false;
	/** internal states */
	private HashMap<Hypothesis, Hypothesis> cachedEnv;	// Reuse saturated hypothesis graph when possible
	
	public ConstraintAnalysis(ConstraintGraph graph, boolean isSym, boolean isVerbose, boolean isRec) {
        this.graph = graph;
        this.isSym = isSym;
        this.isVerbose = isVerbose;
        this.isRec = isRec;
        unsatPaths = new UnsatPaths();
        cachedEnv = new HashMap<Hypothesis, Hypothesis>();
	}

    public UnsatPaths genErrorPaths ( ) {        
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
    	if (isVerbose)
    		System.out.println("graph_size: "+graph.getAllNodes().size());
				
		for (Node start : startNodes) {
			for (Node end : endNodes) {
				Element e1 = start.getElement();
				Element e2 = end.getElement();
				
				if (isSym && (start.getIndex() <= end.getIndex()))
						continue;
				
				List<Edge> l = finder.getPath(start, end, isVerbose);
				if (l==null) continue;
								
				if (!isRec && start.getIndex() != end.getIndex()) {
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
		return unsatPaths;
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
	
    public int getPathNumber () {
    	if (!done)
		genErrorPaths();
	
    	return unsatPaths.size();
    }
}
