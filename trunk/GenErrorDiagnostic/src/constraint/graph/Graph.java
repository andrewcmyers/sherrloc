package constraint.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.graph.visitor.DFSPathVisitor;
import constraint.graph.visitor.NodeVisitor;

public abstract class Graph {
	
	protected List<Node> allNodes = new ArrayList<Node>();
	protected List<Edge> allEdges = new ArrayList<Edge>();
	protected Map<Node, Map<Node, Set<Edge>>>   edges = new HashMap<Node, Map<Node,Set<Edge>>>();
	
	public Graph( ) {
	}
	
	public void addNode (Node n) {
		allNodes.add(n);
		edges.put(n, new HashMap<Node, Set<Edge>>());
	}
	
	public List<Node> getAllNodes () {
		return allNodes;
	}
	
	public int getIndex (Node node) {
		return allNodes.indexOf(node);
	}
      
    public void addEdge (Node from, Node to, Edge edge) {
    	if (!hasEdge(from, to))
    		edges.get(from).put(to, new HashSet<Edge>());
    	edges.get(from).get(to).add(edge);
    	allEdges.add(edge);
    }
    
    public List<Edge> getAllEdges () {
    	return allEdges;
    }
    
    public boolean hasEdge (Node from, Node to) {
    	return edges.get(from).containsKey(to);
    }
    
    
    public Set<Edge> getEdges (Node from, Node to) {
    	return edges.get(from).get(to);
    }
    
    public Map<Node, Set<Edge>> getNeighbors (Node from) {
    	return edges.get(from);
    }
        
//    public Set<List<Node>> getAllPaths (Node start, Node end) {
//        Set<List<Node>> ret = new HashSet<List<Node>>();
//        List<Node> visited = new ArrayList<Node>();
//        acceptForwardR(start, new AllPathFinder(ret, end, false), visited);
//        return ret;
//    }
    
    public List<Node> getPathDFS (Node start, Node end) {
    	DFSPathVisitor finder = new DFSPathVisitor(end, false);
        List<Node> visited = new ArrayList<Node>();
        acceptForward (start, finder, visited);
        return finder.getResults();
    }
    
    public void labelAll ( ) {
    	for (Node n : allNodes)
        		n.shouldprint = true;
    }
    
    public void acceptForward (NodeVisitor v, List<Node> visited) {
        acceptForward(allNodes.get(0), v, visited);
    }
    
    public void acceptForward (Node node, NodeVisitor v, List<Node> visited) {
        if (visited.contains(node))
            return;
        v.discoverVertex(node);
    	v.visit(node);
        visited.add(node);
        Map<Node, Set<Edge>> neighbors = getNeighbors(node);
        for (Node next : neighbors.keySet()) {
            acceptForward(next, v, visited);
        }
        v.leaveVertex(node);
    }
    
    /*
     * A routine written for exhaustive path finding
     * Not used anymore becuause of effectiveness
     */
//    public void acceptForwardR (Node node, NodeVisitor v, List<Node> visited) {
//        if (visited.contains(node))
//            return;
//        v.discoverVertex(node);
//        v.visit(node);
//        visited.add(node);
//        Map<Node, Edge> neighbors = getNeighbors(node);
//        for (Node next : neighbors.keySet()) {
//        	if (neighbors.get(next) instanceof IdEdge)
//        		continue;
//            acceptForwardR(next, v, visited);
//        }
//        v.leaveVertex(node);
//        visited.remove(node);
//    }
}