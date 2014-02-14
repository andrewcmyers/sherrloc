package constraint.analysis;

import graph.Edge;
import graph.Node;

import java.util.List;

/**
 * The interface for path finders
 */
public interface PathFinder {

	/**
	 * Return a path in the constraint graph, when a partial ordering on
	 * <code>start</code> and <code>end</code> is derivable from all constraints
	 * along the returned path
	 * 
	 * @param start
	 *            Node on LHS
	 * @param end
	 *            Node on RHS
	 * @param verbose
	 *            True when collecting data for evaluation
	 * 
	 * @return A constraint path such that <code>start <= end</code> is
	 *         derivable from constraints along the path. Return null if no such
	 *         path exists
	 */
	public List<Edge> getPath(Node start, Node end, boolean verbose);
}
