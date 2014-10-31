package sherrloc.graph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import sherrloc.constraint.analysis.PathFinder;
import sherrloc.constraint.ast.ConstructorApplication;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.ast.Inequality;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.Relation;

/**
 * A path in constraint graph
 */
public class ConstraintPath {
	private List<Edge> edges;
	private Hypothesis assumption;
	private PathFinder finder;

	/**
	 * @param edges
	 *            Edges forming the path
	 * @param finder
	 *            A {@link PathFinder} that contains a saturated graph
	 * @param globalEnv
	 *            Global assumptions
	 * @param cachedEnv
	 *            Saturated hypothesis graphs for better performance
	 */
	public ConstraintPath(List<Edge> edges, PathFinder finder, Hypothesis globalEnv) {
		this.edges = edges;
		assumption = new Hypothesis();
		assumption.addEnv(globalEnv);
		for (Edge edge : edges) {
			for (Inequality ieq : edge.getHypothesis())
				assumption.addInequality(ieq);
		}
		this.finder = finder;
	}
	
	/**
	 * @return Length of the constraint path
	 */
	int length() {
		return edges.size();
	}

	/**
	 * @return All edges along the constraint path
	 */
	public List<Edge> getEdges() {
		return edges;
	}

	/**
	 * @return The weakest hypothesis that makes the constraint path satisfiable. Return null for an empty path
	 */
	public Inequality getMinHypo() {
		if (edges.size() == 0) {
			return null;
		} else
			// hypothesis use elements with no position information. See the
			// definition of getBaseElement for why.
			return new Inequality(getFirstElement().getBaseElement(),
					getLastElement().getBaseElement(), Relation.LEQ);
	}

	/**
	 * @return A list of nodes along the path such that n0 <= n1 <= ... <= nm
	 */
	public List<Node> getLeqNodes() {
		List<Node> ret = new ArrayList<Node>();

		if (edges.size() == 0)
			return ret;

		Node first = getFirst();
		ret.add(first);
		for (int k = 0; k < length(); k++) {
			Edge edge = edges.get(k);
			if (finder.getPath(first, edge.to) != null)
				ret.add(edge.to);
		}
		return ret;
	}

	/**
	 * A satisfiable path is one where all LEQ relations on nodes are satisfiable
	 * whenever there is an LEQ edge on them
	 * 
	 * @return True if the constraint path is a satisfiable path
	 */
	public boolean isInformative() {
		if (edges.size() == 0)
			return false;

		// at least one of the edges should be equation edge, since otherwise,
		// the path is not informative
		boolean hasEqu = false;
		if (getFirst().isGray() || getLast().isGray()) {
			if (getFirstElement() instanceof ConstructorApplication && getLastElement() instanceof ConstructorApplication) {
				ConstructorApplication ca1 = (ConstructorApplication) getFirstElement();
				ConstructorApplication ca2 = (ConstructorApplication) getLastElement();
				if (ca1.getCons().equals(ca2.getCons()))
					return false;
			}
		}
		for (Edge e : edges) {
			if (e instanceof ConstraintEdge) {
				hasEqu = true;
				break;
			}
			if (e.getTo().isGray())
				return false;
		}
		if (!hasEqu)
			return false;
		
		// we'd like to eliminate apparent dependencies on the satisfiability of paths
		// 1. Any path that uses unsat path is not informative
		// 2. If the hypothesis *derives* A->B and B->C, the path from A->C is ignored
		Stack<Element> leqElements = new Stack<Element>();
		Stack<EdgeCondition> conditions = new Stack<EdgeCondition>();
		int length = 0;
		int nestedDummy = 0;
		Element first = getFirstElement();
		leqElements.push(first);

		for (int k = 0; k < edges.size(); k++) {
			Edge edge = edges.get(k);
			Element eto = edge.to.getElement();
			boolean needCmp = !eto.trivialEnd() && !(eto instanceof MeetElement)
					&& !(eto instanceof JoinElement);
			
			if (edge instanceof DummyEdge) {
				boolean isLeft = ((DummyEdge) edge).isLeft;				
				if (isLeft) {
					leqElements.push(eto);
					nestedDummy ++;
				}
				else {
					leqElements.pop();
					nestedDummy --;
				}
				continue;
			}
			
			if (edge instanceof ConstructorEdge) {
				ConstructorEdge cedge = (ConstructorEdge) edge;
				if (conditions.empty() || !conditions.peek().matches(cedge.condition)) {
					conditions.push(cedge.condition);
					leqElements.push(eto);
					needCmp = false;
				}
				else {
					conditions.pop();
					leqElements.pop();
				}
			}
			if (needCmp) {
				if (conditions.size() == 0 
						&& nestedDummy==0 && !eto.hasVars()) {
					if (length > 0)
						return false;
					else
						length++;
				}

				if (!leqElements.empty()
						&& !assumption.satisfiable(leqElements.peek(), eto)
						&& !(leqElements.peek().equals(getFirstElement()) && eto.equals(getLastElement())))
					return false;
				else if (!eto.hasVars()){
					leqElements.pop();
					leqElements.push(eto);
				}
			}
		}
		return true;
	}
	
	/**
	 * @return True if the relation on end nodes start <= end is derivable
	 */
	public boolean isValidPath() {
		if (edges.size() == 0)
			return false;
		
		for (Edge e : edges) {
			if (e instanceof DummyEdge)
				return false;
		}

		return assumption.leq(getFirst().getElement(), getLast().getElement());
	}
	
	/**
	 * @return True if the relation on end nodes start <= end is satisfiable
	 */
	public boolean isSatPath() {
		if (edges.size() == 0)
			return false;

		return assumption.satisfiable(getFirstElement(), getLastElement());
	}

	/**
	 * @return True if the relation on end nodes start <= end is not provable
	 */
	public boolean isUnsatPath() {
		if (edges.size() == 0)
			return false;

		return !isSatPath();
	}

	/**
	 * @return All nodes along the path
	 */
	public Set<Node> getAllNodes() {
		HashSet<Node> ret = new HashSet<Node>();

		if (edges.size() == 0)
			return ret;

		Node first = getFirst();
		ret.add(first);
		for (int k = 0; k < length(); k++) {
			Edge edge = edges.get(k);
			ret.add(edge.to);
		}
		return ret;
	}

	/**
	 * @return First node of the path
	 */
	public Node getFirst() {
		if (edges.size() != 0)
			return edges.get(0).from;
		else
			return null;
	}

	/**
	 * @return Last node of the path
	 */
	public Node getLast() {
		if (edges.size() != 0)
			return edges.get(edges.size() - 1).to;
		else
			return null;
	}

	/**
	 * @return First element on the path
	 */
	public Element getFirstElement() {
		return getFirst().getElement();
	}

	/**
	 * @return Last element on the path
	 */
	public Element getLastElement() {
		return getLast().getElement();
	}

	/**
	 * @return Hypothesis along the path
	 */
	public Hypothesis getAssumption() {
		return assumption;
	}

	/**
	 * Increase the # satisfiable paths using constraint elements and
	 * constraints
	 */
	public void incSuccCounter() {
		if (edges.size() == 0)
			return;

		// avoid duplicate expressions
		// changing the sets to "Node", "Edge" breaks OCaml tests, check this later
		Set<String> processedNodes = new HashSet<String>();
		Set<String> processedEdges = new HashSet<String>();

		Node leftmost = getFirst();
		leftmost.incSuccCounter();
		processedNodes.add(leftmost.toString());
		for (int k = 0; k < length(); k++) {
			Edge edge = edges.get(k);
			if (!processedEdges.contains(edge)) {
				edge.incNumSuccCounter();
				processedEdges.add(edge.toString());
			}
			if (!processedNodes.contains(edge.getTo().toString())) {
				edge.getTo().incSuccCounter();
				processedNodes.add(edge.getTo().toString());
			}
		}
	}

	/**
	 * Mark all elements along the path as contributing to error. Only used for
	 * producing a complete program slice that contributing to errors
	 */
	public void setCause() {
		if (edges.size() == 0)
			return;

		Node leftmost = getFirst();
		leftmost.setCause();
		for (int k = 0; k < length(); k++) {
			Edge edge = edges.get(k);
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
		}
	}

	@Override
	public String toString() {
		String ret = "";

		if (edges.size() == 0)
			return "";

		ret += "\n----Start of one path----\n";
		Node leftmost = getFirst();
		ret += leftmost.getElement().toString() + "\n";
		for (int k = 0; k < length(); k++) {
			Edge edge = edges.get(k);
			ret += "--> (" + (edge.toString()) + ")\n";
			ret += edge.to.getElement().toString() + "\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
