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
	private Set<Inequality> assertions;
	private ConstraintGraph graph;
	private PathFinder finder = null;
	private boolean SHOW_HYPOTHESIS = false;
	private Environment parent = null; // used to reduce shared environments
										// (e.g., to store global assumptions)

	public Environment() {
		assertions = new HashSet<Inequality>();
		graph = new ConstraintGraph(null, new HashSet<Constraint>(), false);
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Inequality cons : assertions)
			sb.append(cons.toString() + "; ");
		sb.append("\n");
		for (Element ele : graph.getAllElements()) {
			sb.append(ele.toString() + "; ");
		}
		return sb.toString();
	}

	/**
	 * Add an inequality to the assumption
	 * 
	 * @param ieq
	 *            An inequality to be added
	 */
	public void addInequality(Inequality ieq) {
		assertions.add(ieq.baseInequality());
	}

	/**
	 * Add all inequalities in <code>e</code> to the assumption
	 * 
	 * @param e
	 *            An assumption to be added
	 */
	public void addEnv(Environment e) {
		if (parent == null) {
			parent = e;
		} else {
			assertions.addAll(e.getInequalities());
		}
	}

	/**
	 * Add all elements in <code>eles</code> to the constraint graph, in order
	 * to infer partial orderings when they does not exist in the graph
	 * 
	 * @param eles
	 *            Elements to be added
	 */
	public void addElements(Set<Element> eles) {
		for (Element e : eles) {
			graph.getNode(e.getBaseElement());
		}
	}

	/**
	 * Return a fresh assumption where an inequality <code>e1</code> <=
	 * <code>e2</code> is added to the current assumption
	 * 
	 * @param e1
	 *            Element on LHS
	 * @param e2
	 *            Element on RHS
	 * @return A fresh assumption where an inequality <code>e1</code> <=
	 *         <code>e2</code> is added to the current assumption
	 */
	public Environment addLeq(Element e1, Element e2) {
		Environment e = new Environment();
		e.addEnv(this);
		e.addInequality(new Inequality(e1.getBaseElement(),
				e2.getBaseElement(), Relation.LEQ));
		return e;
	}

	/**
	 * @return A string of all inequalities
	 */
	public String inequalityString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Assertions:\n");
		for (Inequality e : assertions) {
			sb.append(e + "\n");
		}
		return sb.toString();
	}

	/**
	 * Test if the relation <code>e1</code> <= <code>e2</code> can be derived
	 * from the assumption
	 * 
	 * @param e1
	 *            Element on LHS
	 * @param e2
	 *            Element on RHS
	 * @return True if <code>e1</code> <= <code>e2</code>
	 */
	public boolean leq(Element e1, Element e2) {
		return leq(e1, e2, true);
	}

	/**
	 * See {@link #leq(Element, Element)}
	 */
	private boolean leq(Element p1, Element p2, boolean rec) {
		Element e1 = p1.getBaseElement();
		Element e2 = p2.getBaseElement();

		// for the same constructor, we should break them into components. Just
		// return true here
		if (e1.equals(e2))
			return true;

		if (e1.isBottom() || e2.isTop())
			return true;

		if (e1 instanceof Variable || e2 instanceof Variable) {
			return true;
		}

		// the assumption can be made on the join/meet/constructors, so apply
		// the assumptions first
		if (rec && leqApplyAssertions(e1, e2))
			return true;

		if (e1 instanceof ConstructorApplication
				&& e2 instanceof ConstructorApplication) {
			if (!((ConstructorApplication) e1).getCons().equals(
					((ConstructorApplication) e2).getCons()))
				return false;
		}

		if (e1 instanceof ConstructorApplication
				&& e2 instanceof ConstructorApplication) {
			List<Element> l1 = ((ConstructorApplication) e1).elements;
			List<Element> l2 = ((ConstructorApplication) e2).elements;
			boolean contravariant = ((ConstructorApplication) e1).getCons()
					.isContraVariant();
			for (int i = 0; i < l1.size(); i++) {
				if (!contravariant && !leq(l1.get(i), l2.get(i)))
					return false;
				if (contravariant && !leq(l2.get(i), l1.get(i)))
					return false;
			}
			return true;
		}

		if (e1 instanceof JoinElement) {
			for (Element e : ((JoinElement) e1).getElements())
				if (!leq(e, e2, rec))
					return false;
			return true;
		} else if (e1 instanceof MeetElement) {
			for (Element e : ((MeetElement) e1).getElements())
				if (leq(e, e2, rec))
					return true;
			return false;
		}

		if (e2 instanceof JoinElement) {
			for (Element e : ((JoinElement) e2).getElements())
				if (leq(e1, e, rec))
					return true;
			return false;
		} else if (e2 instanceof MeetElement) {
			for (Element e : ((MeetElement) e2).getElements())
				if (!leq(e1, e, rec))
					return false;
			return true;
		}

		return false;
	}

	/**
	 * @return All inequalities made in the assumption
	 */
	private Set<Inequality> getInequalities() {
		if (parent == null)
			return assertions;
		else {
			Set<Inequality> ret = new HashSet<Inequality>();
			ret.addAll(assertions);
			ret.addAll(parent.getInequalities());
			return ret;
		}
	}

	/**
	 * TODO: is there a way to eliminate the constraints _->_ <= labels?
	 * Otherwise, hard to tell the type of "labels"
	 */
	/**
	 * Saturate a constraint graph from all assumptions to test if
	 * <code>e1</code><=<code>e2</code> can be inferred
	 * 
	 * @param e1
	 *            Element on LHS
	 * @param e2
	 *            Element on RHS
	 * @return True if <code>e1</code><=<code>e2</code> can be inferred from
	 *         assumption
	 */
	private boolean leqApplyAssertions(Element e1, Element e2) {

		if (finder == null) {
			for (Inequality c : getInequalities()) {
				graph.addOneInequality(c);
			}
			graph.generateGraph();

			if (SHOW_HYPOTHESIS) {
				graph.labelAll();
				System.out.println(graph.toDotString());
			}
			finder = new ShortestPathFinder(graph);
		}

		if (graph.hasElement(e1) && graph.hasElement(e2)) {
			if (finder.getPath(graph.getNode(e1), graph.getNode(e2), false) != null)
				return true;
			for (Element e : graph.getAllElements()) {
				if (finder.getPath(graph.getNode(e1), graph.getNode(e), false) != null
						&& leq(e, e2, false))
					return true;
			}
			return false;
		} else
			return false;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Environment) {
			Environment other = (Environment) obj;
			for (Inequality cons : assertions) {
				if (!other.assertions.contains(cons))
					return false;
			}

			for (Inequality cons : other.assertions) {
				if (!assertions.contains(cons))
					return false;
			}
		}
		return true;
	}

	@Override
	public int hashCode() {
		int ret = 0;
		for (Inequality cons : assertions)
			ret += cons.hashCode();
		return ret;
	}

}
