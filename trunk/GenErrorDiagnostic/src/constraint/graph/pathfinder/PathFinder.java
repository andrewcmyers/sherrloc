package constraint.graph.pathfinder;

import java.util.List;

import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.Node;

/** 
 * 
 * A path finder is initialized with a constraint graph and it provides an interface 
 * that returns an unsatisfiable path, if one exists
 * 
 */

public abstract class PathFinder {
	boolean initialized = false;
	ConstraintGraph g;
	
	public PathFinder(ConstraintGraph graph) {
		g = graph;
	}
	
	abstract void initialize ( );
	
	abstract protected List<Edge> _getPath (Node start, Node end);
	
	public List<Edge> getPath (Node start, Node end) {
		if (!initialized) {
			long startTime = System.currentTimeMillis();
			initialize();
			initialized = true;
			long endTime = System.currentTimeMillis();
			System.out.println("path_finding time: " + (endTime-startTime));
		}
		
		return _getPath(start, end);
	}
}
