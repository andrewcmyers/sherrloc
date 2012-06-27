package constraint.graph.pathfinder;

import java.util.List;

import constraint.graph.Edge;
import constraint.graph.Graph;
import constraint.graph.Node;

/** 
 * 
 * A path finder is initialized with a constraint graph and it provides an interface 
 * that returns an unsatisfiable path, if one exists
 * 
 */

public abstract class PathFinder {
	boolean initialized = false;
	Graph g;
	
	public PathFinder(Graph graph) {
		g = graph;
	}
	
	abstract void initialize ( );
	
	abstract protected List<Edge> _getPath (Node start, Node end);
	
	public List<Edge> getPath (Node start, Node end) {
		if (!initialized) {
			initialize();
			initialized = true;
		}
		
		return _getPath(start, end);
	}
}
