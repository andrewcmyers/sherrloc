package constraint.ast;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.graph.ConstraintGraph;
import constraint.graph.ElementNode;
import constraint.graph.Node;
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
	boolean SHOW_HYPOTHESIS = false;
	
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
	
	/* unified version */
	public boolean leq(Element e1, Element e2) {
		
		// for the same constructor, we should break them into components. Just return true here
		if (e1.equals(e2))
			return true;
		
		if (e1.isDecomposable() && e2.isDecomposable()) {
			List<Element> l1 = ((EnumerableElement) e1).elements;
			List<Element> l2 = ((EnumerableElement) e2).elements;
			for (int i=0; i<l1.size(); i++) {
				if (!leq(l1.get(i), l2.get(i)))
					return false;
			}
			return true;
		}
		
		if (e1 instanceof Bottom || e2 instanceof Top)
			return true;
		
		// the assumption can be made on the join/meet
		if (leqApplyAssertions(e1, e2))
			return true;

		// e1 leq any element of e2
		if (e2 instanceof JoinElement) {
			for (Element e : ((JoinElement)e2).getElements())
				if (leq(e1, e)) 
					return true;
			return false;
		}
		// e1 leq all elements of e2
		else if (e2 instanceof MeetElement) {
			for (Element e : ((MeetElement)e2).getElements())
				if (!leq(e1, e)) 
					return false;
			return true;
		}

		return leqApplyAssertions(e1, e2);
	}
	
	/* earlier version */
//	public boolean leq(Element e1, Element e2) {
//		if (e1.leq_(e2, this))
//			return true;
//			
//		if (e2 instanceof LabelElement) {
//			if (((LabelElement) e2).isTop())
//				return true;
//		}
//		
//		if (leqApplyAssertions(e1, e2))
//			return true;
//		
//		// e1 leq any element of e2
//		if (e2 instanceof JoinElement) {
//			for (Element e : ((JoinElement)e2).getElements())
//				if (this.leq(e1, e)) 
//					return true;
//			return false;
//		}
//		// e1 leq all elements of e2
//		else if (e2 instanceof MeetElement) {
//			for (Element e : ((MeetElement)e2).getElements())
//				if (!this.leq(e1, e)) 
//					return false;
//			return true;
//		}
//		
//        if (e1.leq_(e2, this)) {
//            return true;
//        }
//        
//        // try to use assertions
//        return leqApplyAssertions(e1, e2);
//	}
	
	private boolean leqApplyAssertions(Element e1, Element e2) {
			
		if (finder == null) {
			graph.generateGraph();
			
			if (SHOW_HYPOTHESIS) {
				graph.labelAll();
				System.out.println( graph.toDotString());
			}
			finder = new ShortestPathFinder(graph);
		}

		if (graph.hasElement(e1) && graph.hasElement(e2))
//			for (Element e : graph.getAllElements()) {
//				if (finder.getPath(graph.getNode(e1), graph.getNode(e))!=null && e.leq_(e2, this))
//					return true;
//			}
//		}
//		return false;
			return finder.getPath(graph.getNode(e1), graph.getNode(e2))!=null;
		else
			return false;
    }

	// record the acts for relation
	public void addActsFor(String s1, String s2) {
		if (!ph.containsKey(s1))
			ph.put(s1, new HashSet<String>());
		ph.get(s1).add(s2);
	}
	
	public Set<Node> geqSet(Node start) {
		return finder.geqSet(start);
	}
	
	public Set<Node> leqSet(Node end) {
		return finder.leqSet(end);
	}
		
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Environment) {
			if (assertionString().equals(((Environment) obj).assertionString()))
				return true;
			if (ph.equals(((Environment) obj).ph))
				return true;
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return assertionString().hashCode()+ph.hashCode();
	}

}
