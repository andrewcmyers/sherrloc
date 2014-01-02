package constraint.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
        
    public void labelAll ( ) {
    	for (Node n : allNodes)
        		n.shouldprint = true;
    }
}
