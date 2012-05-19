package constraint.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import constraint.graph.visitor.DFSPathFinder;
import constraint.graph.visitor.LabellingVisitor;
import constraint.graph.visitor.NodeVisitor;

public abstract class Graph {
	
	protected List<Node> allNodes = new ArrayList<Node>();
	protected List<Edge> allEdges = new ArrayList<Edge>();
	protected Map<Node, Map<Node, Edge>>   edges = new HashMap<Node, Map<Node,Edge>>();
	// Edges used in CFL-reachablity algorithm. Should not be observable for most graph operations
	protected Map<Node, Map<Node, List<ReductionEdge>>>   reductionEdges = new HashMap<Node, Map<Node,List<ReductionEdge>>>();
	
	public Graph( ) {
	}
	
	public void addNode (Node n) {
		allNodes.add(n);
		edges.put(n, new HashMap<Node, Edge>());
		reductionEdges.put(n, new HashMap<Node, List<ReductionEdge>>());
	}
	
	public List<Node> getAllNodes () {
		return allNodes;
	}
	
	public int getIndex (Node node) {
		return allNodes.indexOf(node);
	}
      
    public void addEdge (Node from, Node to, Edge edge) {
    	edges.get(from).put(to, edge);
    	allEdges.add(edge);
    }
    
    public List<Edge> getAllEdges () {
    	return allEdges;
    }
    
    protected void addReductionEdge (Node from, Node to, ReductionEdge edge) {
    	if (!reductionEdges.get(from).containsKey(to)) {
    		reductionEdges.get(from).put(to, new ArrayList<ReductionEdge>());
    	}
    	reductionEdges.get(from).get(to).add(edge);
    }
    
    protected List<Edge> getAllReductionEdges ( ) {
    	List<Edge> ret =  new ArrayList<Edge>();
    	for (Node n : allNodes) {
    		for (Node next : reductionEdges.get(n).keySet()) {
    			ret.addAll(reductionEdges.get(n).get(next));
    		}
    	}
    	return ret;
    }
    
    protected List<ReductionEdge> getReductionEdges (Node from, Node to) {
    	if (reductionEdges.get(from).containsKey(to))
    		return reductionEdges.get(from).get(to);
    	else
    		return new ArrayList<ReductionEdge>();
    }
    
    protected IdEdge getIdEdge (Node from, Node to) {
    	List<ReductionEdge> edges = getReductionEdges(from, to);
    	for (Edge edge : edges) {
    		if (edge instanceof IdEdge)
    			return ((IdEdge)edge);
    	}
    	return null;
    }
    
    public boolean hasEdge (Node from, Node to) {
    	return edges.get(from).containsKey(to);
    }
    
    protected boolean hasReductionEdge (Edge edge) {
    	Node from = edge.from;
    	Node to = edge.to;

    	if (reductionEdges.get(from).containsKey(to)) {
    		for (Edge e : reductionEdges.get(from).get(to)) {
    			if (e.equals (edge)) {
    				return true;
    			}
    		}
    	}
    	return false;
    }
    
    public Edge getEdge (Node from, Node to) {
    	return edges.get(from).get(to);
    }
    
    public Map<Node, Edge> getNeighbors (Node from) {
    	return edges.get(from);
    }
    
    protected Map<Node, List<ReductionEdge>> getReductionNeighbors (Node from) {
    	return reductionEdges.get(from);
    }
        
//    public Set<List<Node>> getAllPaths (Node start, Node end) {
//        Set<List<Node>> ret = new HashSet<List<Node>>();
//        List<Node> visited = new ArrayList<Node>();
//        acceptForwardR(start, new AllPathFinder(ret, end, false), visited);
//        return ret;
//    }
    
    public List<Node> getPathDFS (Node start, Node end) {
    	DFSPathFinder finder = new DFSPathFinder(end, false);
        List<Node> visited = new ArrayList<Node>();
        acceptForward (start, finder, visited);
        return finder.getResults();
    }
    
    public void labelAll ( ) {
        List<Node> visited = new ArrayList<Node>();
        acceptForward(new LabellingVisitor(), visited);
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
        Map<Node, Edge> neighbors = getNeighbors(node);
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
    
	public void acceptForwardReductionGraph (Node node, NodeVisitor v, List<Node> visited) {
		if (visited.contains(node))
			return;
		v.discoverVertex(node);
		v.visit(node);
		visited.add(node);
		Map<Node, List<ReductionEdge>> neighbors = reductionEdges.get(node);
		for (Node next : neighbors.keySet()) {
			acceptForward(next, v, visited);
		}
		v.leaveVertex(node);
	}
	
	public void acceptForwardReductionGraph (NodeVisitor v, List<Node> visited) {
        acceptForwardReductionGraph(allNodes.get(0), v, visited);
    }
}
