package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.Graph;
import constraint.graph.Node;
import constraint.graph.visitor.AllPathVisitor;

/**
 * 
 * This class exclusively finds all valid paths between two nodes
 *
 */

public class AllPathFinder extends PathFinder {

	public AllPathFinder(Graph graph) {
		super(graph);
	}
	
	void initialize() {
		// do nothing
	}

	@Override
	protected List<Edge> _getPath(Node start, Node end) {
		Set<List<Node>> result = new HashSet<List<Node>>();
		List<Node> visited = new ArrayList<Node>();
		AllPathVisitor v = new AllPathVisitor(result, end, false);
		g.acceptForward(start, v, visited);
		for (List<Node> l : result) {
//			ConstraintPath p = new ConstraintPath(l);
		}
		return null;
	}

}
