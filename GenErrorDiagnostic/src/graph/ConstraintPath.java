package graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import constraint.analysis.PathFinder;
import constraint.ast.Constructor;
import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.Hypothesis;
import constraint.ast.Inequality;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Relation;

public class ConstraintPath {
	List<Edge> edges;
	Hypothesis assumption;
	PathFinder finder;
	Inequality minHypo;

	public ConstraintPath(List<Edge> edges, PathFinder finder,
			Hypothesis globalEnv, HashMap<Hypothesis, Hypothesis> cachedEnv) {
		this.edges = edges;
		assumption = new Hypothesis();
		assumption.addEnv(globalEnv);
		for (Edge edge : edges) {
			for (Inequality ieq : edge.getInequalities())
				assumption.addInequality(ieq);
		}
		assumption = getEnv_(cachedEnv);
		this.finder = finder;
		if (edges.size() == 0) {
			this.minHypo = null;
		} else
			// hypothesis use elements with no position information. See the
			// definition of getBaseElement for why.
			this.minHypo = new Inequality(getFirstElement().getBaseElement(),
					getLastElement().getBaseElement(), Relation.LEQ);
	}

	int size() {
		return edges.size();
	}

	public List<Edge> getEdges() {
		return edges;
	}

	public Inequality getMinHypo() {
		return minHypo;
	}

	public List<Node> getIdNodes() {
		ArrayList<Node> ret = new ArrayList<Node>();

		if (edges.size() == 0)
			return ret;

		// System.out.println("Checking one equation in env: "+path.env);
		Node first = getFirst();
		ret.add(first);

		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			if (finder.getPath(first, edge.to, false) != null)
				ret.add(edge.to);
		}
		return ret;
	}

	// a succ path is one where all LEQ are provable and all LEQ nodes are
	// variable free
	public boolean isSuccPath() {
		if (edges.size() == 0)
			return false;

		// at least one of the edges should be equation edge
		boolean hasEqu = false;
		for (Edge e : edges) {
			if (e instanceof EquationEdge) {
				hasEqu = true;
				break;
			}
		}
		if (!hasEqu)
			return false;

		// System.out.println("Checking one equation in env: "+path.env);
		Stack<Node> leqNodes = new Stack<Node>();
		Stack<EdgeCondition> conditions = new Stack<EdgeCondition>();
		int length = 0;
		Node first = getFirst();
		leqNodes.push(first);

		for (int k = 0; k < edges.size(); k++) {
			Edge edge = edges.get(k);
			Node eto = edge.to;
			boolean needCmp = eto.getElement() instanceof Constructor
					|| eto.getElement() instanceof ConstructorApplication
					|| eto.getElement() instanceof JoinElement
					|| eto.getElement() instanceof MeetElement;
			if (edge instanceof EquationEdge || edge instanceof JoinEdge
					|| edge instanceof MeetEdge) {
				if (needCmp) {
					if (conditions.size() == 0) {
						if (length > 0)
							return false;
						else
							length++;
					}

					if (!assumption.leq(leqNodes.peek().getElement(),
							eto.getElement()))
						return false;
					else {
						leqNodes.pop();
						leqNodes.push(eto);
					}
				}
			} else if (edge instanceof ConstructorEdge) {
				ConstructorEdge cedge = (ConstructorEdge) edge;
				if (conditions.empty()
						|| !conditions.peek().matches(cedge.condition)) {
					conditions.push(cedge.condition);
					leqNodes.push(eto);
				} else {
					conditions.pop();
					leqNodes.pop();
					if (needCmp) {
						if (conditions.size() == 0) {
							if (length > 0)
								return false;
							else
								length++;
						}

						if (!leqNodes.empty()
								&& !assumption.leq(
										leqNodes.peek().getElement(),
										eto.getElement()))
							return false;
						else {
							leqNodes.pop();
							leqNodes.push(eto);
						}
					}
				}
			}
		}

		return true;
	}

	public boolean isUnsatPath() {
		if (edges.size() == 0)
			return false;

		return !assumption.leq(getFirst().getElement(), getLast().getElement());
	}

	private Hypothesis getEnv_(HashMap<Hypothesis, Hypothesis> cachedEnv) {
		if (cachedEnv.containsKey(assumption))
			return cachedEnv.get(assumption);
		else {
			cachedEnv.put(assumption, assumption);
			return assumption;
		}
	}

	public Set<Node> getAllNodes() {
		HashSet<Node> ret = new HashSet<Node>();

		if (edges.size() == 0)
			return ret;

		Node first = getFirst();
		ret.add(first);

		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret.add(edge.to);
		}
		return ret;
	}

	public Node getFirst() {
		if (edges.size() != 0)
			return edges.get(0).from;
		else
			return null;
	}

	public Element getFirstElement() {
		return getFirst().getElement();
	}

	public Element getLastElement() {
		return getLast().getElement();
	}

	public Node getLast() {
		if (edges.size() != 0)
			return edges.get(edges.size() - 1).to;
		else
			return null;
	}

	public Hypothesis getAssumption() {
		return assumption;
	}

	public void setAssumption(Hypothesis assumption) {
		this.assumption = assumption;
	}

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
		for (int k = 0; k < size(); k++) {
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

	public void setCause() {
		if (edges.size() == 0)
			return;

		Node leftmost = getFirst();
		leftmost.setCause();
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
		}
	}

	public String toString() {
		String ret = "";

		if (edges.size() == 0)
			return "";

		ret += "\n----Start of one path----\n";
		Node leftmost = getFirst();
		ret += leftmost.getElement().toString() + "\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret += "--> (" + (edge.toString()) + ")\n";
			ret += edge.to.getElement().toString() + "\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
