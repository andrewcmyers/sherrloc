package constraint.analysis;

import graph.ConstraintGraph;
import diagnostic.UnsatPaths;

/**
 * Interface of constraint analysis, which identifies satisfiable/unsatisfiable
 * constraints in a constraint graph
 */
public interface ConstraintAnalysis {

	/**
	 * Identifies satisfiable/unsatisfiable constraints in a constraint graph:
	 * the unsatisfiable paths are returned; the information of # satisfiable
	 * paths using graph nodes/edges is updated in the constraint graph
	 * 
	 * @param graph
	 *            A constraint graph to be analyzed. Information of #
	 *            satisfiable using graph nodes/edges will be updated in the
	 *            constraint graph after analysis
	 * 
	 * @return Identified unsatisfiable paths
	 */
	public UnsatPaths genErrorPaths(ConstraintGraph graph);
}
