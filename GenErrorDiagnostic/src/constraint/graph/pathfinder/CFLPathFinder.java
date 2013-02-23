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
import constraint.graph.EquationEdge;
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
	protected Map<Node, List<Node>>   joinElements = new HashMap<Node, List<Node>>();
	protected Map<Node, List<Node>>   meetElements = new HashMap<Node, List<Node>>();
	boolean[][] hasRightEdge;

	
	public CFLPathFinder(ConstraintGraph graph) {
		super(graph);
		// initialize reduction edges
		for (Node n : graph.getAllNodes()) {
			reductionEdges.put(n, new HashMap<Node, List<ReductionEdge>>());
			joinElements.put(n, new ArrayList<Node>());
			meetElements.put(n, new ArrayList<Node>());
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
		
		// if all components flows into another constructor's components, add an additional edge
		Set<Node> consSet = new HashSet<Node>();
		for (Node n : g.getAllNodes()) {
			if (n instanceof ElementNode && ((ElementNode)n).getElement() instanceof ComplexElement) {
				consSet.add(n);
			}
		}
		
		for (Node n1 : consSet) {
			for (Node n2 : consSet) {
				if (n1.equals(n2) || getIdEdge(n1, n2)!=null) continue;

				ComplexElement e1 = (ComplexElement) ((ElementNode)n1).getElement();
				ComplexElement e2 = (ComplexElement) ((ElementNode)n2).getElement();
				
				if (e1.getCons().equals(e2.getCons())) {
					boolean success = true;
					String exp = "";

					Edge edge = null;
					for (int i=0; i<e1.getCons().getArity(); i++) {
						Element comp1 = e1.getElements().get(i);
						Element comp2 = e2.getElements().get(i);
						
						if ( comp1.equals(comp2))
							continue;
						
						edge = getIdEdge(g.getNode(e1.getElements().get(i)), g.getNode(e2.getElements().get(i)));
						if (edge==null) {
							success = false;
							break;
						}
						exp += comp1.toString() + "<=" +comp2.toString();
					}
					
					if (success) {
						List<Edge> trace = new ArrayList<Edge>();
						if (edge!=null)
							trace.add(new CompEdge(n1, n2, edge.getAssumption(), exp));
						else
							trace.add(new CompEdge(n1, n2, new Environment(), exp));
						addReductionEdge(n1, n2, new IdEdge(n1, n2, trace));
					}
				}
			}
		}
		
		// now handle the join on RHS and meet on LHS
		for (Node n : g.getAllNodes()) {
			if (((ElementNode)n).getElement() instanceof JoinElement) {
				JoinElement je = (JoinElement) ((ElementNode)n).getElement();
				for (Element ele : je.getElements()) {
					joinElements.get(g.getNode(ele)).add(n);
				}
			}
			
			if (((ElementNode)n).getElement() instanceof MeetElement) {
				MeetElement je = (MeetElement) ((ElementNode)n).getElement();
				for (Element ele : je.getElements()) {
					meetElements.get(g.getNode(ele)).add(n);
				}
			}
		}

		saturation();
		
//		// now we handle meet and join labels
//		List<ElementNode> meetNodes = new ArrayList<ElementNode>();
//		List<ElementNode> joinNodes = new ArrayList<ElementNode>();
//		
//		for (Node n : g.getAllNodes()) {
//			ElementNode en = (ElementNode) n;
//			if (en.getElement() instanceof MeetElement)
//				meetNodes.add(en);
//			if (en.getElement() instanceof JoinElement)
//				joinNodes.add(en);
//		}
		
		// handle meet nodes first
//		for (ElementNode n : meetNodes) {
//			// if a node flows into all components, then we add an edge to n
//			List<Element> comp = ((MeetElement)n.getElement()).getElements();
//			boolean success = true;
//			for (Node candidate : g.getAllNodes()) {
//				if (candidate.equals(n)) continue;
//				
//				for (int i=0; i<comp.size(); i++) {
//					if (getIdEdge(candidate, g.getNode(comp.get(i)))==null) {
//						success = false;
//						break;
//					}
//				}
//				
//				if (success) {
//					System.out.println("@@@@@@@@@ "+((ElementNode)candidate).getElement() + "->" + n.getElement());
//					addReductionEdge(candidate, n, new IdEdge(candidate, n, new ArrayList<Edge>()));
//				}
//			}
//		}
//		
//		for (ElementNode n : joinNodes) {
//			// if all components flows into node candidate, then we add an edge from n to candidate
//			List<Element> comp = ((JoinElement)n.getElement()).getElements();
//			boolean success = true;
//			for (Node candidate : g.getAllNodes()) {
//				if (candidate.equals(n)) continue;
//				
//				for (int i=0; i<comp.size(); i++) {
//					if (getIdEdge(g.getNode(comp.get(i)), candidate)==null) {
//						success = false;
//						break;
//					}
//				}
//				
//				if (success) {
//					System.out.println("@@@@@@@@@ "+ n.getElement() + "->" + ((ElementNode)candidate).getElement());
//					addReductionEdge(n, candidate, new IdEdge(n, candidate, new ArrayList<Edge>()));
//				}
//			}
//		}
//		
//		saturation();
	}

	// the method used to generate all CFG nonterminals in a graph
	abstract public void saturation();
}
