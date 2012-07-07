package constraint.ast;

import java.util.ArrayList;
import java.util.List;

import constraint.graph.ConstraintGraph;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;

/**
 * Environment is a List of additional assumptions (in the form of equations)
 * That can be used in the leq check
 */
public class Environment {
	List<Equation> assertions;
	ConstraintGraph graph;
	PathFinder finder = null;
	
	public Environment() {
		assertions = new ArrayList<Equation>();
		graph = new ConstraintGraph(null, assertions, false);
	}
	
	public void addAssertion (Equation equ) {
		assertions.add(equ);
	}
	
	public boolean actsFor (String e1, String e2) {
		if (e2.equals("_"))
			return true;
		if (e1.equals("*"))
			return true;
		return e1.equals(e2);
	}
	
	public void printAssertions () {
		for (Equation e : assertions) {
			System.out.println(e);
		}
	}
	
	public boolean leq(Element e1, Element e2) {
		if (e1.equals(e2))
			return true;
		
//		// do some easy tests firsts.
//        if (L1.isBottom()) return true;
//        //if (L2.isBottom()) return false;
//        
//        if (L2.isTop()) return true;
//        if (L1.isTop()) return false;
		
		// e1 leq any element of e2
		if (e2 instanceof JoinElement) {
			for (Element e : ((JoinElement)e2).getElements())
				if (this.leq(e1, e)) 
					return true;
			return false;
		}
		// e1 leq all elements of e2
		else if (e2 instanceof MeetElement) {
			for (Element e : ((MeetElement)e2).getElements())
				if (this.leq(e1, e)) 
					return false;
			return true;
		}
		
        if (e1.leq_(e2)) {
            return true;
        }
        
        // try to use assertions
//        return false;
        return leqApplyAssertions(e1, e2);
	}
	
	private boolean leqApplyAssertions(Element e1, Element e2) {
		if (finder == null) {
			graph.generateGraph();
			finder = new ShortestPathFinder(graph);
		}
		
		if (graph.hasElement(e1) && graph.hasElement(e2))
			return finder.getPath(graph.getNode(e1), graph.getNode(e2))!=null;
		else
			return false;
    }
}
