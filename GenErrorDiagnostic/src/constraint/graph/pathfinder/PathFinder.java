package constraint.graph.pathfinder;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
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
			initialize();
			initialized = true;
		}
		
		return _getPath(start, end);
	}
	
	public Set<Node> geqSet(ElementNode start) {
		Set<Node> ret = new HashSet<Node>();
		ret.add(start);
		
		if (!g.hasElement(start.getElement()))
			return ret;
		
		// this is the node representing same element in assumption graph
		Node s = g.getNode(start.getElement());
		for (Node node : g.getAllNodes()) {
			ElementNode en = (ElementNode)node;
			if ( !en.getElement().hasVars() && !(en.getElement().toString().equals("(*)->(*)")) && _getPath(s, node)!=null) {
				System.out.println(en.getElement());
				ret.add(node);
			}
		}
		return ret;
	}
	
	public Set<Node> leqSet(ElementNode end) {
		Set<Node> ret = new HashSet<Node>();
		ret.add(end);

		if (!g.hasElement(end.getElement()))
			return ret;

		// this is the node representing same element in assumption graph
		Node e = g.getNode(end.getElement());
		for (Node node : g.getAllNodes()) {
			ElementNode en = (ElementNode)node;
			if (!en.getElement().hasVars() && !(en.getElement().toString().equals("(_)->(_)")) && _getPath(node, e)!=null) {
				System.out.println(en.getElement());
				ret.add(node);
			}
		}
		return ret;
	}
}
