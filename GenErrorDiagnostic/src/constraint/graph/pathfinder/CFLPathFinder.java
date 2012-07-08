package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import constraint.graph.ConstructorEdge;
import constraint.graph.Edge;
import constraint.graph.EquationEdge;
import constraint.graph.Graph;
import constraint.graph.IdEdge;
import constraint.graph.JoinEdge;
import constraint.graph.LeftEdge;
import constraint.graph.MeetEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

/**
 * 
 * Constract a CFL graph based on the input
 *
 */
abstract public class CFLPathFinder extends PathFinder {
	
	// Edges used in CFL-reachablity algorithm. Should not be observable for most graph operations
	protected Map<Node, Map<Node, List<ReductionEdge>>>   reductionEdges = new HashMap<Node, Map<Node,List<ReductionEdge>>>();
	boolean[][] hasRightEdge;

	
	public CFLPathFinder(Graph graph) {
		super(graph);
		// initialize reduction edges
		for (Node n : graph.getAllNodes()) {
			reductionEdges.put(n, new HashMap<Node, List<ReductionEdge>>());
		}
		hasRightEdge = new boolean[graph.getAllNodes().size()][graph.getAllNodes().size()];
	}
	
	protected void addReductionEdge (Node from, Node to, ReductionEdge edge) {
    	if (!reductionEdges.get(from).containsKey(to)) {
    		reductionEdges.get(from).put(to, new ArrayList<ReductionEdge>());
    	}
    	reductionEdges.get(from).get(to).add(edge);
    }
    
    protected List<ReductionEdge> getAllReductionEdges ( ) {
    	List<ReductionEdge> ret =  new ArrayList<ReductionEdge>();
    	for (Node n : g.getAllNodes()) {
    		for (Node next : reductionEdges.get(n).keySet()) {
    			ret.addAll(reductionEdges.get(n).get(next));
    		}
    	}
    	return ret;
    }
    
    protected boolean hasReductionEdge (Edge edge) {
    	Node from = edge.getFrom();
    	Node to = edge.getTo();

    	if (reductionEdges.get(from).containsKey(to)) {
    		for (Edge e : reductionEdges.get(from).get(to)) {
    			if (e.equals (edge)) {
    				return true;
    			}
    		}
    	}
    	return false;
    }
    
    protected Map<Node, List<ReductionEdge>> getReductionNeighbors (Node from) {
    	return reductionEdges.get(from);
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
    
    protected RightEdge getRightEdge (Node from, Node to) {
    	List<ReductionEdge> edges = getReductionEdges(from, to);
    	for (Edge edge : edges) {
    		if (edge instanceof RightEdge)
    			return ((RightEdge)edge);
    	}
    	return null;
    }
    
//	public void acceptForwardReductionGraph (Node node, NodeVisitor v, List<Node> visited) {
//		if (visited.contains(node))
//			return;
//		v.discoverVertex(node);
//		v.visit(node);
//		visited.add(node);
//		Map<Node, List<ReductionEdge>> neighbors = reductionEdges.get(node);
//		for (Node next : neighbors.keySet()) {
//			acceptForward(next, v, visited);
//		}
//		v.leaveVertex(node);
//	}
//	
//	public void acceptForwardReductionGraph (NodeVisitor v, List<Node> visited) {
//        acceptForwardReductionGraph(allNodes.get(0), v, visited);
//    }

	@Override
	void initialize() {
		
		List<Edge> edges = g.getAllEdges();
		for (Node start : g.getAllNodes()) {
			for (Node end : g.getAllNodes()) {
				hasRightEdge[g.getIndex(start)][g.getIndex(end)] = false;
			}
		}

		// generate the initial CFG graph
		for (Edge edge : edges) {
			List<Edge> list = new ArrayList<Edge>();
			Node from = edge.getFrom();
			Node to   = edge.getTo();
			
			list.add(edge);
			
			// add equation edge as "id" edge, constructor edge as left or right edge
			if (edge instanceof EquationEdge || edge instanceof MeetEdge || edge instanceof JoinEdge) {
				addReductionEdge(from, to, new IdEdge(from, to, list));
			}
			else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				if (e.getCondition().isReverse()) {
					addReductionEdge(from, to, new RightEdge(e.getCondition(), from, to, list));
					hasRightEdge[g.getIndex(from)][g.getIndex(to)] = true;
				}
				else {
					addReductionEdge(from, to, new LeftEdge (e.getCondition(), from, to, list));
				}
			}
		}
		
		saturation();
	}

	// the method used to generate all CFG nonterminals in a graph
	abstract public void saturation();
}
