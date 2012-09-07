package constraint.graph.pathfinder;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

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
	
	public Set<Node> geqSet(Node start) {
		Set<Node> ret = new HashSet<Node>();
		ret.add(start);
		
		if (!g.getAllNodes().contains(start))
			return ret;
		
		for (Node node : g.getAllNodes()) {
			if ( _getPath(start, node)!=null)
				ret.add(node);
		}
		return ret;
	}
	
	public Set<Node> leqSet(Node end) {
		Set<Node> ret = new HashSet<Node>();
		ret.add(end);

		if (!g.getAllNodes().contains(end))
			return ret;

		for (Node node : g.getAllNodes()) {
			if (_getPath(node, end)!=null)
				ret.add(node);
		}
		return ret;
	}
}
