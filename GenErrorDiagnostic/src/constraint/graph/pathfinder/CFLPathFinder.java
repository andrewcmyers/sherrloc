package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.ast.ComplexElement;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.graph.CompEdge;
import constraint.graph.ConstraintGraph;
import constraint.graph.ConstructorEdge;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.EmptyEdge;
import constraint.graph.EquationEdge;
import constraint.graph.JoinEdge;
import constraint.graph.LeftEdge;
import constraint.graph.LeqEdge;
import constraint.graph.MeetEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

/**
 * 
 * Construct a CFL graph based on the input
 *
 */
abstract public class CFLPathFinder extends PathFinder {
	
	// Edges used in CFL-reachablity algorithm. Should not be observable for most graph operations
	protected Map<Node, Map<Node, List<ReductionEdge>>>   reductionEdges = new HashMap<Node, Map<Node,List<ReductionEdge>>>();
	protected Map<Node, List<Node>>   joinElements = new HashMap<Node, List<Node>>();
	protected Map<Node, List<Node>>   meetElements = new HashMap<Node, List<Node>>();
	protected Map<Node, List<Node>>   consElements = new HashMap<Node, List<Node>>();
	boolean[][] hasRightEdge;

	
	public CFLPathFinder(ConstraintGraph graph) {
		super(graph);
		// initialize reduction edges
		for (Node n : graph.getAllNodes()) {
			reductionEdges.put(n, new HashMap<Node, List<ReductionEdge>>());
			joinElements.put(n, new ArrayList<Node>());
			meetElements.put(n, new ArrayList<Node>());
			consElements.put(n, new ArrayList<Node>());
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
    
    protected LeqEdge getLeqEdge (Node from, Node to) {
    	List<ReductionEdge> edges = getReductionEdges(from, to);
    	for (Edge edge : edges) {
    		if (edge instanceof LeqEdge)
    			return ((LeqEdge)edge);
    	}
    	return null;
    }
    
    protected Set<RightEdge> getRightEdges (Node from, Node to) {
    	Set<RightEdge> ret = new HashSet<RightEdge>();
    	List<ReductionEdge> edges = getReductionEdges(from, to);
    	for (Edge edge : edges) {
    		if (edge instanceof RightEdge)
    			ret.add((RightEdge)edge);
    	}
    	return ret;
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
				hasRightEdge[start.getIndex()][end.getIndex()] = false;
			}
		}

		// generate the initial CFG graph
		for (Edge edge : edges) {
			Node from = edge.getFrom();
			Node to   = edge.getTo();
			
			// add equation edge as "id" edge, constructor edge as left or right edge
			if (edge instanceof EquationEdge || edge instanceof MeetEdge || edge instanceof JoinEdge) {
				addReductionEdge(from, to, new LeqEdge(from, to, edge, EmptyEdge.getInstance()));
			}
			else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				if (e.getCondition().isReverse()) {
					addReductionEdge(from, to, new RightEdge(e.getCondition(), from, to, edge, EmptyEdge.getInstance()));
					hasRightEdge[from.getIndex()][to.getIndex()] = true;
				}
				else {
					addReductionEdge(from, to, new LeftEdge (e.getCondition(), from, to, edge, EmptyEdge.getInstance()));
				}
			}
		}		
		
		// initialize the lookup tables
		for (Node n : g.getAllNodes()) {
			Element element = ((ElementNode)n).getElement();
			if (element instanceof JoinElement) {
				JoinElement je = (JoinElement) element;
				for (Element ele : je.getElements()) {
					joinElements.get(g.getNode(ele)).add(n);
				}
			}
			
			if (element instanceof MeetElement) {
				MeetElement je = (MeetElement) element;
				for (Element ele : je.getElements()) {
					meetElements.get(g.getNode(ele)).add(n);
				}
			}
			
			if (element instanceof ComplexElement) {
				ComplexElement ce = (ComplexElement) element;
				for (Element ele : ce.getElements()) {
					consElements.get(g.getNode(ele)).add(n);
				}
			}
		}

		saturation();
	}

	// the method used to generate all CFG nonterminals in a graph
	abstract public void saturation();
}
