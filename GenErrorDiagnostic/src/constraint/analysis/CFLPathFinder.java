package constraint.analysis;

import graph.ConstraintGraph;
import graph.ConstructorEdge;
import graph.Edge;
import graph.EdgeCondition;
import graph.EmptyEdge;
import graph.EquationEdge;
import graph.JoinEdge;
import graph.LeftEdge;
import graph.LeqEdge;
import graph.MeetEdge;
import graph.Node;
import graph.ReductionEdge;
import graph.RightEdge;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Saturate a constraint graph according to a context-free-grammar with three
 * types of edges:
 * <ul>
 * <li>LEQ: an inequality on node
 * <li>LEFT: constructor edge
 * <li>RIGHT: destructor edge
 * </ul>
 * <p>
 * See the full grammar in paper "Toward General Diagnosis of Static Errors" by
 * Danfeng Zhang and Andrew C. Myers
 */
abstract public class CFLPathFinder implements PathFinder {
	/** Edges used in CFL-reachablity algorithm */
	protected LeqEdge[][] leqPath;
	protected Map<EdgeCondition, LeftEdge>[][] leftPath;
	// since the RIGHT edges are rare in a graph, and no right edges are
	// inferred, using HashMap can be more memory efficient than arrays
	protected Map<Node, Map<Node, List<RightEdge>>> rightPath;

	/** other fields */
	protected boolean initialized = false;
	protected final ConstraintGraph g;

	/**
	 * @param graph
	 *            A graph to be saturated
	 */
	public CFLPathFinder(ConstraintGraph graph) {
		g = graph;
		int size = g.getAllNodes().size();
		leqPath = new LeqEdge[size][size];
		leftPath = new HashMap[size][size];
		rightPath = new HashMap<Node, Map<Node, List<RightEdge>>>();
		for (Node start : g.getAllNodes()) {
			for (Node end : g.getAllNodes()) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();

				leqPath[sIndex][eIndex] = null;
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition, LeftEdge>();
			}
		}
	}

	/**
	 * Add a {@link ReductionEdge} to the graph
	 * 
	 * @param edge
	 *            An edge to be added
	 */
	abstract protected void addEdge(ReductionEdge edge);

	protected List<RightEdge> getRightEdges(Node from, Node to) {
		if (rightPath.containsKey(from) && rightPath.get(from).containsKey(to)) {
			return rightPath.get(from).get(to);
		} else
			return new ArrayList<RightEdge>();
	}

	/**
	 * Convert all graph edges into {@link ReductionEdge}s
	 */
	private void initialize() {

		List<Edge> edges = g.getAllEdges();

		for (Edge edge : edges) {
			Node from = edge.getFrom();
			Node to = edge.getTo();

			if (edge instanceof EquationEdge || edge instanceof MeetEdge
					|| edge instanceof JoinEdge) {
				addEdge(new LeqEdge(from, to, edge, EmptyEdge.getInstance()));
			} else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				if (e.getCondition().isReverse()) {
					addEdge(new RightEdge(e.getCondition(), from, to, edge,
							EmptyEdge.getInstance()));
				} else {
					addEdge(new LeftEdge(e.getCondition(), from, to, edge,
							EmptyEdge.getInstance()));
				}
			}
		}
	}

	/**
	 * Return a path in the constraint graph so that a partial ordering on
	 * <code>start, end</code> can be derived from constraints along the path.
	 * Return null when no such path exits
	 * 
	 * @param verbose
	 *            Set true to output saturation time
	 */
	public List<Edge> getPath(Node start, Node end, boolean verbose) {
		if (!initialized) {
			long startTime = System.currentTimeMillis();
			initialize();
			saturation();
			initialized = true;
			long endTime = System.currentTimeMillis();
			if (verbose)
				System.out.println("path_finding time: " + (endTime - startTime));
		}

		LeqEdge path = getLeqPath(start, end);
		if (path != null)
			return path.getEdges();
		else
			return null;
	}

	/**
	 * Return an LEQ path from <code>start</code> to <code>end</code>
	 * 
	 * @param start
	 *            Start node
	 * @param end
	 *            End node
	 * @return An LEQ path
	 */
	protected LeqEdge getLeqPath(Node start, Node end) {
		int sIndex = start.getIndex();
		int eIndex = end.getIndex();

		return leqPath[sIndex][eIndex];
	}

	/**
	 * Saturate the constraint graph
	 */
	abstract protected void saturation();
}
