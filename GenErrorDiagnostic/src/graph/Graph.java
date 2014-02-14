package graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public abstract class Graph {
	
	protected List<Node> allNodes = new ArrayList<Node>();
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
	      
    public void addEdge (Edge edge) {
    	Node from = edge.from;
    	Node to = edge.to;
    	if (!hasEdge(from, to))
    		edges.get(from).put(to, new HashSet<Edge>());
    	edges.get(from).get(to).add(edge);
    }
    
    public List<Edge> getAllEdges () {
    	List<Edge> ret = new ArrayList<Edge>();
    	for (Node n1 : edges.keySet()) {
    		for (Node n2 : edges.get(n1).keySet()) {
    			ret.addAll(edges.get(n1).get(n2));
    		}
    	}
    	return ret;
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
