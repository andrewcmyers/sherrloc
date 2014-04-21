package sherrloc.constraint.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import sherrloc.graph.ConstraintEdge;
import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.ConstructorEdge;
import sherrloc.graph.DummyEdge;
import sherrloc.graph.Edge;
import sherrloc.graph.EdgeCondition;
import sherrloc.graph.JoinEdge;
import sherrloc.graph.LeqCondition;
import sherrloc.graph.LeqRevCondition;
import sherrloc.graph.MeetEdge;
import sherrloc.graph.Node;
import sherrloc.graph.ReductionEdge;
import sherrloc.graph.RightEdge;

/**
 * Saturate a constraint graph according to a context-free-grammar with three
 * types of edges:
 * <ul>
 * <li>LEQ: an inequality on node
 * <li>LEFT: constructor edge
 * <li>RIGHT: destructor edge
 * </ul>
 * <p>
 * See the full grammar in the paper "Toward General Diagnosis of Static Errors"
 * by Danfeng Zhang and Andrew C. Myers
 */
abstract public class CFLPathFinder implements PathFinder {
	/** Edges used in CFL-reachablity algorithm */
	protected Map<EdgeCondition, List<Triple>>[][] nextHop;
	// since the RIGHT edges are rare in a graph, and no right edges are
	// inferred, using HashMap can be more memory efficient than arrays
	protected Map<Integer, Map<Integer, List<RightEdge>>> rightPath;

	/** other fields */
	protected boolean initialized = false;
	protected final ConstraintGraph g;

	public class Triple {
		public Node start, end;
		public EdgeCondition ty;
		
		public Triple(Node start, Node end, EdgeCondition type) {
			this.start = start;
			this.end = end;
			this.ty = type;
		}
	}
	
	public enum EDGE_TYPE {LEQ, LEFT, RIGHT};
	/**
	 * @param graph
	 *            A graph to be saturated
	 */
	public CFLPathFinder(ConstraintGraph graph) {
		g = graph;
		int size = g.getAllNodes().size();
		nextHop = new Map[size][size];
		rightPath = new HashMap<Integer, Map<Integer, List<RightEdge>>>();
		for (Node start : g.getAllNodes()) {
			for (Node end : g.getAllNodes()) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();

				nextHop[sIndex][eIndex] = null;
			}
		}
	}

	/**
	 * Add a {@link ReductionEdge} to the graph
	 * 
	 * @param edge
	 *            An edge to be added
	 */
	abstract protected void inferEdge(Node start, Node end, EdgeCondition inferredType, int size, List<Triple> evidence);
	
	/**
	 * Return all {@link RightEdge}s from <code>fIndex</code> to
	 * <code>tIndex</code>
	 * 
	 * @param fIndex
	 *            Start node
	 * @param tIndex
	 *            End node
	 * @return All {@link RightEdge}s from <code>fIndex</code> to
	 *         <code>tIndex</code>
	 */
	protected List<RightEdge> getRightEdges(int fIndex, int tIndex) {
		if (hasRightEdges(fIndex, tIndex)) {
			return rightPath.get(fIndex).get(tIndex);
		} else
			return new ArrayList<RightEdge>();
	}
	
	/**
	 * Return true if there is at least one {@link RightEdge} from
	 * <code>fIndex</code> to <code>tIndex</code>
	 * 
	 * @param fIndex
	 *            Start node
	 * @param tIndex
	 *            End node
	 * @return True if there is at least one {@link RightEdge} from
	 *         <code>fIndex</code> to <code>tIndex</code>
	 */
	protected boolean hasRightEdges(int fIndex, int tIndex) {
		if (rightPath.containsKey(fIndex) && rightPath.get(fIndex).containsKey(tIndex)) {
			return true;
		} 
		else
			return false;
	}
	
	/**
	 * Convert all graph edges into {@link ReductionEdge}s
	 */
	private void initialize() {

		List<Edge> edges = g.getAllEdges();

		for (Edge edge : edges) {
			if (edge instanceof ConstraintEdge || edge instanceof MeetEdge
					|| edge instanceof JoinEdge) {
				inferEdge(edge.getFrom(), edge.getTo(), LeqCondition.getInstance(), 1, new ArrayList<Triple>());
			} else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				inferEdge(edge.getFrom(), edge.getTo(), e.getCondition(), 1, new ArrayList<Triple>());
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

		List<Edge> path = new ArrayList<Edge>();
		getLeqPath(start, end, LeqCondition.getInstance(), path, false);
		return path;
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
	protected void getLeqPath(Node start, Node end, EdgeCondition ec, List<Edge> ret, boolean isRev) {
		int sIndex = start.getIndex();
		int eIndex = end.getIndex();
		if (ec instanceof LeqRevCondition) {
			sIndex = end.getIndex();
			eIndex = start.getIndex();
			ec = LeqCondition.getInstance();
			isRev = !isRev;
		}
		
		if (ec instanceof LeqCondition && (nextHop[sIndex][eIndex] == null || !nextHop[sIndex][eIndex].containsKey(ec)))
			return;
		assert (nextHop[sIndex][eIndex] != null && nextHop[sIndex][eIndex].containsKey(ec));
		
		List<Triple> evis = nextHop[sIndex][eIndex].get(ec);
			
		// base condition
		if (evis.isEmpty()) {
			Edge current = null;
			for (Edge edge : g.getEdges(start, end)) {
				if (ec instanceof LeqCondition
						&& (edge instanceof JoinEdge
								|| edge instanceof MeetEdge || edge instanceof ConstraintEdge)) {
					current = edge;
				} else if (edge instanceof ConstructorEdge) {
					ConstructorEdge ce = (ConstructorEdge) edge;
					if (ce.getCondition().equals(ec))
						current = edge;
				}
				if (current != null)
					break;
			}
			assert (current != null);
			if (isRev) {
				current = current.getReverse();
			}
			ret.add(current);
		}
		else {
			if (!isRev) {
				Node prev = start;
				for (Triple evidence : evis) {
					if (!evidence.start.equals(prev))
						ret.add(new DummyEdge(start, evidence.start));
					getLeqPath(evidence.start, evidence.end, evidence.ty, ret, isRev);
					prev = evidence.end;
				}
				if (!prev.equals(end))
					ret.add(new DummyEdge(prev, end));
			}
			else {
				ListIterator<Triple> ite = evis.listIterator(evis.size());
				Node prev = start;
				while (ite.hasPrevious()) {
					Triple evidence = ite.previous();
					if (!evidence.start.equals(prev))
						ret.add(new DummyEdge(start, evidence.start));
					getLeqPath(evidence.start, evidence.end, evidence.ty, ret, isRev);
					prev = evidence.end;
				}
				if (!prev.equals(end))
					ret.add(new DummyEdge(prev, end));
			}
		}
	}

	/**
	 * Saturate the constraint graph
	 */
	abstract protected void saturation();
}
