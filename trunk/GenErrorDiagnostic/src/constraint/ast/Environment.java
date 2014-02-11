package constraint.ast;

import graph.ConstraintGraph;
import graph.pathfinder.PathFinder;
import graph.pathfinder.ShortestPathFinder;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Environment represents hypothesis (a conjunction of inequalities)
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
	
	public boolean leq(Element p1, Element p2) {
		return leq(p1, p2, true);
	}
	
	private boolean leq(Element p1, Element p2, boolean rec) {
		Element e1 = p1.getBaseElement();
		Element e2 = p2.getBaseElement();
				
		// for the same constructor, we should break them into components. Just return true here
		if (e1.equals(e2))
			return true;
		
		if (e1.isBottom() || e2.isTop())
			return true;
		
		if (e1 instanceof Variable || e2 instanceof Variable) {
			return true;
		}
		
		// the assumption can be made on the join/meet
		if (rec && leqApplyAssertions(e1, e2))
			return true;
					
		if (e1 instanceof ConstructorApplication && e2 instanceof ConstructorApplication) {
			if (!((ConstructorApplication)e1).getCons().equals(((ConstructorApplication)e2).getCons()))
				return false;
		}
		
		if (e1 instanceof ConstructorApplication && e2 instanceof ConstructorApplication) {
			List<Element> l1 = ((ConstructorApplication) e1).elements;
			List<Element> l2 = ((ConstructorApplication) e2).elements;
			boolean contravariant = ((ConstructorApplication)e1).getCons().isContraVariant();
			for (int i=0; i<l1.size(); i++) {
				if (!contravariant && !leq(l1.get(i), l2.get(i)))
					return false;
				if (contravariant && !leq(l2.get(i), l1.get(i)))
					return false;
			}		
			return true;
		}
		
		if (e1 instanceof JoinElement) {
			for (Element e : ((JoinElement)e1).getElements())
				if (!leq(e, e2, rec)) 
					return false;
			return true;
		}
		else if (e1 instanceof MeetElement) {
			for (Element e : ((MeetElement)e1).getElements())
				if (leq(e, e2, rec)) 
					return true;
			return false;
		}
				
		if (e2 instanceof JoinElement) {
			for (Element e : ((JoinElement)e2).getElements())
				if (leq(e1, e, rec)) 
					return true;
			return false;
		}
		else if (e2 instanceof MeetElement) {
			for (Element e : ((MeetElement)e2).getElements())
				if (!leq(e1, e, rec)) 
					return false;
			return true;
		}
		
		return false;
		
//		return (rec && leqApplyAssertions(e1, e2));
	}
		
	/* 
	 * TODO: is there a way to eliminate the constraints _->_ <= labels? Otherwise, hard to tell the type of "labels"
	 */
	private boolean leqApplyAssertions(Element e1, Element e2) {
			
		if (finder == null) {
			for (Constraint c : assertions) {
				graph.addOneConstraint(c);
			}
			graph.generateGraph();
			
			if (SHOW_HYPOTHESIS) {
				graph.labelAll();
				System.out.println( graph.toDotString());
			}
			finder = new ShortestPathFinder(graph);
		}

		if (graph.hasElement(e1) && graph.hasElement(e2)) {
			if (finder.getPath(graph.getNode(e1), graph.getNode(e2), false)!=null)
				return true;
			for (Element e : graph.getAllElements()) {
				if (finder.getPath(graph.getNode(e1), graph.getNode(e), false)!=null && leq(e, e2, false))
					return true;
			}
			return false;
		}
		else
			return false;
    }
			
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Environment) {
			Environment other = (Environment) obj;
			for (Constraint cons : assertions) {
				if (!other.assertions.contains(cons))
					return false;
			}
			
			for (Constraint cons : other.assertions) {
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
