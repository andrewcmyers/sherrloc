package constraint.ast;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.AttemptGoal;

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
	boolean SHOW_HYPOTHESIS = false;
	
	public Environment() {
		assertions = new HashSet<Constraint>();
		graph = new ConstraintGraph(null, assertions, false);
	}
	
	public Set<Constraint> getAssertions() {
		return assertions;
	}
	
	public void addAssertion (Constraint equ) {
		assertions.add(equ);
	}
	
	public void addEnv (Environment e) {
		for (Constraint c : e.assertions) {
			assertions.add(c);
		}
	}
	
	public Environment addLeq (Element e1, Element e2) {
		Environment e = new Environment();
		e.addEnv(this);
		e.addAssertion(new Constraint(e1, e2, Relation.LEQ, null, null));
		return e;
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
		
		if ( e1 instanceof Variable || e2 instanceof Variable)
			return true;
		
		if (e1 instanceof Constructor && e2 instanceof Constructor) {
			if (((Constructor)e1).sameas(e2))
				return true;
		}
		
		if (e1 instanceof ConstructorElement && e2 instanceof ConstructorElement) {
			if (!((ConstructorElement)e1).getCons().sameas(((ConstructorElement)e2).getCons()))
				return false;
		}
		
		// the assumption can be made on the join/meet
		if (leqApplyAssertions(e1, e2))
			return true;

		if (e1 instanceof ConstructorElement && e2 instanceof ConstructorElement) {
			List<Element> l1 = ((ConstructorElement) e1).elements;
			List<Element> l2 = ((ConstructorElement) e2).elements;
			boolean contravariant = ((ConstructorElement)e1).getCons().isContraVariant();
			for (int i=0; i<l1.size(); i++) {
				if (!contravariant && !leq(l1.get(i), l2.get(i)))
					return false;
				if (contravariant && !leq(l2.get(i), l1.get(i)))
					return false;
			}
			return true;
		}
		
		if (e1 instanceof Bottom || e2 instanceof Top)
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
	
	public Set<Node> geqSet(ElementNode start) {
		return finder.geqSet(start);
	}
	
	public Set<Node> leqSet(ElementNode end) {
		return finder.leqSet(end);
	}
		
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Environment) {
			Environment other = (Environment) obj;
			for (Constraint cons : assertions) {
				if (!other.getAssertions().contains(cons))
					return false;
			}
			
			for (Constraint cons : other.getAssertions()) {
				if (!assertions.contains(cons))
					return false;
			}
		}
		return true;
	}
	
	@Override
	public int hashCode() {
		int ret = 0;
		for (Constraint cons : assertions)
			ret += cons.toString().hashCode();
		return ret;
	}

}
