package constraint.ast;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import constraint.graph.ConstraintGraph;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;

/**
 * Environment is a List of additional assumptions (in the form of equations)
 * That can be used in the leq check
 */
public class Environment {
	Set<Constraint> assertions;
	ConstraintGraph graph;
	PathFinder finder = null;
	Map<String, Set<String>> ph;
	
	public Environment() {
		assertions = new HashSet<Constraint>();
		graph = new ConstraintGraph(null, assertions, false);
		ph = new HashMap<String, Set<String>>();
	}
	
	public void addAssertion (Constraint equ) {
		assertions.add(equ);
	}
	
	public void addEnv (Environment e) {
		for (Constraint c : e.assertions) {
			assertions.add(c);
		}
	}
	
	public boolean actsFor (String e1, String e2) {
		// do simple checks
		if (e2.equals("_"))
			return true;
		if (e1.equals("*"))
			return true;
		
		if (e1.equals(e2))
			return true;
		
		// TODO: transitivity is not handled yet
		if (ph.containsKey(e1)) {
			return ph.get(e1).contains(e2);
		}
		return false;
	}
	
	public String assertionString () {
		StringBuffer sb = new StringBuffer();
		sb.append("Assertions:\n");
		for (Constraint e : assertions) {
			sb.append(e+"\n");
		}
		return sb.toString();
	}
	
	public boolean leq(Element e1, Element e2) {
		if (e1.equals(e2))
			return true;
			
		if (e2 instanceof LabelElement) {
			if (((LabelElement) e2).isTop())
				return true;
		}
		
		if (leqApplyAssertions(e1, e2))
			return true;
		
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
		
        if (e1.leq_(e2, this)) {
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

		if (graph.hasElement(e1)) {
			for (Element e : graph.getAllElements()) {
				if (finder.getPath(graph.getNode(e1), graph.getNode(e))!=null && e.leq_(e2, this))
					return true;
			}
		}
		return false;
//			return finder.getPath(graph.getNode(e1), graph.getNode(e2))!=null;
//		else
//			return false;
    }

	// record the acts for relation
	public void addActsFor(String s1, String s2) {
		if (!ph.containsKey(s1))
			ph.put(s1, new HashSet<String>());
		ph.get(s1).add(s2);
	}

	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Environment) {
			return assertionString().equals(((Environment) obj).assertionString());
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return assertionString().hashCode();
	}

}
