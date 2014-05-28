package sherrloc.constraint.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
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
	protected Map<EdgeCondition, Set<List<Triple>>>[][] nextHop;
	// since the RIGHT edges are rare in a graph, and no right edges are
	// inferred, using HashMap can be more memory efficient than arrays
	protected Map<Integer, Map<Integer, List<RightEdge>>> rightPath;
	private Map<Integer, Set<Integer>> inferredLR;

	/** other fields */
	protected final ConstraintGraph g;

	public class Triple {
		public Node start, end;
		public EdgeCondition ty;
		
		public Triple(Node start, Node end, EdgeCondition type) {
			this.start = start;
			this.end = end;
			this.ty = type;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof Triple) {
				Triple other = (Triple) obj;
				return start.equals(other.start) && end.equals(other.end) && ty.equals(other.ty);
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			return start.hashCode()*1021+end.hashCode()*71+ty.hashCode();
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
		inferredLR = new HashMap<Integer, Set<Integer>>();
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
	abstract protected void inferEdge(Node start, Node end, EdgeCondition inferredType, int size, List<Triple> evidence, boolean isInit);
	
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
	protected void initialize() {

		List<Edge> edges = g.getAllEdges();

		for (Edge edge : edges) {
			if (edge instanceof ConstraintEdge || edge instanceof MeetEdge
					|| edge instanceof JoinEdge) {
				if (!edge.getFrom().equals(edge.getTo()))
					inferEdge(edge.getFrom(), edge.getTo(), LeqCondition.getInstance(), 1, new ArrayList<Triple>(), true);
			} else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				inferEdge(edge.getFrom(), edge.getTo(), e.getCondition(), 1, new ArrayList<Triple>(), true);
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
	public Set<List<Edge>> getPath(Node start, Node end) {
		Set<List<Edge>> ret = new HashSet<List<Edge>>();
		getLeqPath(start, end, LeqCondition.getInstance(), ret, false);
		return ret;
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
	protected void getLeqPath(Node start, Node end, EdgeCondition ec, Set<List<Edge>> ret, boolean isRev) {
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
		
		for (List<Triple> evis : nextHop[sIndex][eIndex].get(ec)) {
			List<Edge> lst = new ArrayList<Edge>();
		// base condition
		if (evis.isEmpty()) {
			Edge current = getOriginalEdge(start, end, ec);
			if (isRev) {
				current = current.getReverse();
			}
			lst.add(current);
		}
		else {
			if (!isRev) {
				Node prev = start;
				for (Triple evidence : evis) {
					if (!evidence.start.equals(prev))
						lst.add(new DummyEdge(start, evidence.start));
					Set<List<Edge>> eset = new HashSet<List<Edge>>();
					getLeqPath(evidence.start, evidence.end, evidence.ty, eset, isRev);
					for (List<Edge> elst : eset) {
						lst.addAll(elst);
					}
					prev = evidence.end;
				}
				if (!prev.equals(end))
					lst.add(new DummyEdge(prev, end));
			}
			else {
				ListIterator<Triple> ite = evis.listIterator(evis.size());
				Node prev = start;
				while (ite.hasPrevious()) {
					Triple evidence = ite.previous();
					if (!evidence.start.equals(prev))
						lst.add(new DummyEdge(start, evidence.start));
					Set<List<Edge>> eset = new HashSet<List<Edge>>();
					getLeqPath(evidence.start, evidence.end, evidence.ty, eset, isRev);
					for (List<Edge> elst : eset) {
						lst.addAll(elst);
					}
					prev = evidence.end;
				}
				if (!prev.equals(end))
					lst.add(new DummyEdge(prev, end));
			}
		}
		ret.add(lst);
		}
	}
	
	/**
	 * Add an atomic LEQ edge to the graph
	 * 
	 * @param startIdx
	 *            Index of start node
	 * @param endIdx
	 *            Index of end node
	 */
	protected void addAtomicLeqEdge (int startIdx, int endIdx) {
		if (!inferredLR.containsKey(startIdx))
			inferredLR.put(startIdx, new HashSet<Integer>());
		inferredLR.get(startIdx).add(endIdx);
	}
	
	/**
	 * Return true if there is an atomic LEQ edge in the graph
	 * 
	 * @param startIdx
	 *            Index of start node
	 * @param endIdx
	 *            Index of end node
	 * @return True if there is an atomic LEQ edge in the graph
	 */
	protected boolean hasAtomicLeqEdge (int startIdx, int endIdx) {
		return inferredLR.containsKey(startIdx) && inferredLR.get(startIdx).contains(endIdx);
	}
	
	/**
	 * Saturate the constraint graph
	 */
	abstract protected void saturation();
	
	protected Edge getOriginalEdge (Node start, Node end, EdgeCondition type) {
		if (g.getEdges(start, end)==null)
			return null;
		for (Edge edge : g.getEdges(start, end)) {
			if (type instanceof LeqCondition
					&& (edge instanceof JoinEdge
							|| edge instanceof MeetEdge || edge instanceof ConstraintEdge)) {
				return edge;
			} else if (edge instanceof ConstructorEdge) {
				ConstructorEdge ce = (ConstructorEdge) edge;
				if (ce.getCondition().equals(type))
					return edge;
			}
		}
		return null;
	}
	
	public static boolean isDashedEdge (Node start) {
		boolean isJoin = start.getElement() instanceof JoinElement;
		return !start.getElement().trivialEnd() && !isJoin;
	}
	
	public static boolean isSolidEdge (Node start) {
		boolean isMeet = start.getElement() instanceof MeetElement;
		return !isDashedEdge(start) || isMeet;
	}
}
