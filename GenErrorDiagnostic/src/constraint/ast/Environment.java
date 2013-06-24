package constraint.ast;

import java.util.HashSet;
import java.util.List;
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
	boolean SHOW_HYPOTHESIS = false;
	
	public Environment() {
		assertions = new HashSet<Constraint>();
		graph = new ConstraintGraph(null, assertions, false);
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Constraint cons : assertions)
			sb.append(cons.toString()+"; ");
		sb.append("\n");
		for (Element ele :graph.getAllElements()) {
			sb.append(ele.toString()+"; ");
		}
		return sb.toString();
	}
	
	public Set<Constraint> getAssertions() {
		return assertions;
	}
	
	public void addAssertion (Constraint equ) {
		assertions.add(equ.baseConstraint());
	}
	
	public void addEnv (Environment e) {
		for (Constraint c : e.assertions) {
			assertions.add(c);
		}
		addElements(e.graph.getAllElements());
	}
	
	public void addElements (Set<Element> eles) {
		for (Element e : eles) {
			graph.getNode(e.getBaseElement());
		}
	}
	
	public Environment addLeq (Element e1, Element e2) {
		Environment e = new Environment();
		e.addEnv(this);
		e.addAssertion(new Constraint(e1.getBaseElement(), e2.getBaseElement(), Relation.LEQ, null, Position.EmptyPosition()));
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
	public boolean leq(Element p1, Element p2) {
		return leq(p1, p2, true);
	}
	
	public boolean leq(Element p1, Element p2, boolean rec) {
		Element e1 = p1.getBaseElement();
		Element e2 = p2.getBaseElement();
		
//		if (e1.toString().contains("C_L") && e2.toString().contains("* -> s3")) {
//			for (Constraint c : assertions) {
//				System.out.println(c.getFirstElement().toString()+c.getFirstElement().hashCode()+ " "+c.getSecondElement()+c.getSecondElement().hashCode());
//			}
//			System.out.println(e1.toString()+e1.hashCode()+" "+e2.toString()+e2.hashCode());
//		}
		
		// for the same constructor, we should break them into components. Just return true here
		if (e1.equals(e2))
			return true;
		
		if (e1.isBottom() || e2.isTop())
			return true;

		
		// the assumption can be made on the join/meet
		if (rec && leqApplyAssertions(e1, e2))
			return true;
		
		// the type to be inferred cannot have recursive types
		if ( e1 instanceof Variable) {
			if (e2.getVars().contains(e1))
				return false;
			else
				return true;
		}
		
		if (e2 instanceof Variable) {
			if (e1.getVars().contains(e2))
				return false;
			else
				return true;
		}
			
		if (e1 instanceof ComplexElement && e2 instanceof ComplexElement) {
			if (!((ComplexElement)e1).getCons().equals(((ComplexElement)e2).getCons()))
				return false;
		}
		
		if (e1 instanceof ComplexElement && e2 instanceof ComplexElement) {
			List<Element> l1 = ((ComplexElement) e1).elements;
			List<Element> l2 = ((ComplexElement) e2).elements;
			boolean contravariant = ((ComplexElement)e1).getCons().isContraVariant();
			for (int i=0; i<l1.size(); i++) {
				if (!contravariant && !leq(l1.get(i), l2.get(i)))
					return false;
				if (contravariant && !leq(l2.get(i), l1.get(i)))
					return false;
			}
			return true;
		}
				
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
		
		return (rec && leqApplyAssertions(e1, e2));
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
	
	/* 
	 * TODO: is there a way to eliminate the constraints _->_ <= labels? Otherwise, hard to tell the type of "labels"
	 */
	private boolean leqApplyAssertions(Element e1, Element e2) {
			
		if (finder == null) {
			for (Constraint c : assertions) {
				graph.addOneConstraint(c.e1, c.e2, c);
			}
			graph.generateGraph();
			
			if (SHOW_HYPOTHESIS) {
				graph.labelAll();
				System.out.println( graph.toDotString());
			}
			finder = new ShortestPathFinder(graph);
		}

		if (graph.hasElement(e1) && graph.hasElement(e2)) {
			if (finder.getPath(graph.getNode(e1), graph.getNode(e2))!=null)
				return true;
			for (Element e : graph.getAllElements()) {
				if (finder.getPath(graph.getNode(e1), graph.getNode(e))!=null && leq(e, e2,false))
					return true;
			}
			return false;
		}
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
