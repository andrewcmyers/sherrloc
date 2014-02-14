package constraint.analysis;

import graph.ConstraintGraph;
import graph.Edge;
import graph.Node;

import java.util.List;

/**
 * A path finder is used to saturate a constraint graph
 */
public abstract class PathFinder {
	protected boolean initialized = false;
	protected final ConstraintGraph g;

	public PathFinder(ConstraintGraph graph) {
		g = graph;
	}

	abstract void initialize();

	abstract protected List<Edge> _getPath(Node start, Node end);

	public List<Edge> getPath(Node start, Node end, boolean verbose) {
		if (!initialized) {
			long startTime = System.currentTimeMillis();
			initialize();
			initialized = true;
			long endTime = System.currentTimeMillis();
			if (verbose)
				System.out.println("path_finding time: "
						+ (endTime - startTime));
		}

		return _getPath(start, end);
	}
}
