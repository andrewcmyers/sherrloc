package constraint.analysis;

import graph.ConstraintGraph;
import graph.ConstructorEdge;
import graph.Edge;
import graph.EdgeCondition;
import graph.EmptyEdge;
import graph.EquationEdge;
import graph.JoinEdge;
import graph.LeftEdge;
import graph.LeqEdge;
import graph.MeetEdge;
import graph.Node;
import graph.ReductionEdge;
import graph.RightEdge;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;

/**
 * Construct a CFL graph based on the input
 */
abstract public class CFLPathFinder implements PathFinder {
	protected boolean initialized = false;
	protected final ConstraintGraph g;
	
	int MAX = 10000;
	ReductionEdge[][] idPath;
	Map<EdgeCondition, LeftEdge>[][] leftPath;
	int[][] shortestLEQ;
	Map<EdgeCondition, Integer>[][] shortestLeft;
	PriorityQueue<ReductionEdge> queue;
	
	/** Edges used in CFL-reachablity algorithm */
	protected Map<Node, Map<Node, List<RightEdge>>> rightEdges = new HashMap<Node, Map<Node,List<RightEdge>>>();
	
	/** Lookup tables to find enumerable elements from components */
	protected Map<Node, List<Node>>   joinElements = new HashMap<Node, List<Node>>();
	protected Map<Node, List<Node>>   meetElements = new HashMap<Node, List<Node>>();
	protected Map<Node, List<Node>>   consElements = new HashMap<Node, List<Node>>();
	
	public CFLPathFinder(ConstraintGraph graph) {
		g = graph;
		queue = new PriorityQueue<ReductionEdge>(
				500, new Comparator<ReductionEdge>() {
					public int compare(ReductionEdge o1, ReductionEdge o2) {
						return o1.size - o2.size;
					}
				});
	}
	
	protected void addLeqEdge (LeqEdge edge) {
		int fIndex = edge.getFrom().getIndex();
		int tIndex = edge.getTo().getIndex();
		
		shortestLEQ[fIndex][tIndex] = 1;
		idPath[fIndex][tIndex] = edge;
		queue.offer(edge);
	}
	
	protected void addLeftEdge (LeftEdge edge) {
		int fIndex = edge.getFrom().getIndex();
		int tIndex = edge.getTo().getIndex();
		
		shortestLeft[fIndex][tIndex].put(edge.cons, 1);
		leftPath[fIndex][tIndex].put(edge.cons, edge);
		queue.offer(edge);
	}
	
	/**
	 * Add <code>edge</code> to the saturated graph
	 * 
	 * @param Edge to be added
	 */
	protected void addRightEdge (RightEdge edge) {
		Node from = edge.getFrom();
		Node to = edge.getTo();
		
		if (!rightEdges.containsKey(from)) {
			rightEdges.put(from, new HashMap<Node, List<RightEdge>>());
		}
		if (!rightEdges.get(from).containsKey(to)) {
			rightEdges.get(from).put(to, new ArrayList<RightEdge>());
		}
		rightEdges.get(from).get(to).add(edge);
	}
            
    protected List<RightEdge> getRightEdges (Node from, Node to) {
		if (rightEdges.containsKey(from) && rightEdges.get(from).containsKey(to)) {
			return rightEdges.get(from).get(to);
		} else
			return new ArrayList<RightEdge>();
    }
    
	void initialize() {
		List<Node> allNodes = g.getAllNodes();
		
		// initialize reduction edges
//		for (Node n : allNodes) {
//			rightEdges.put(n, new HashMap<Node, List<RightEdge>>());
//			joinElements.put(n, new ArrayList<Node>());
//			meetElements.put(n, new ArrayList<Node>());
//			consElements.put(n, new ArrayList<Node>());
//		}
		
		int size = allNodes.size();
		shortestLEQ = new int[size][size];
		shortestLeft = new HashMap[size][size];
		
		// Step 1: initialize graph, fix MAX later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();

				idPath[sIndex][eIndex] = null;
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition, LeftEdge>();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestLEQ[sIndex][eIndex] = MAX;
			}
		}

		for (Node n : allNodes) {
			shortestLEQ[n.getIndex()][n.getIndex()] = 0;
			ReductionEdge e = new LeqEdge(n, n, EmptyEdge.getInstance(),
					EmptyEdge.getInstance());
			idPath[n.getIndex()][n.getIndex()] = e;
		}
		
		List<Edge> edges = g.getAllEdges();

		// generate the initial CFG graph
		for (Edge edge : edges) {
			Node from = edge.getFrom();
			Node to   = edge.getTo();
			
			// add equation edge as "id" edge, constructor edge as left or right edge
			if (edge instanceof EquationEdge || edge instanceof MeetEdge || edge instanceof JoinEdge) {
				addLeqEdge(new LeqEdge(from, to, edge, EmptyEdge.getInstance()));
			}
			else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				if (e.getCondition().isReverse()) {
					addRightEdge(new RightEdge(e.getCondition(), from, to, edge, EmptyEdge.getInstance()));
				}
				else {
					addLeftEdge(new LeftEdge (e.getCondition(), from, to, edge, EmptyEdge.getInstance()));
				}
			}
		}		
		
		// initialize the lookup tables
		for (Node n : g.getAllNodes()) {
			Element element = n.getElement();
			if (element instanceof JoinElement) {
				JoinElement je = (JoinElement) element;
				for (Element ele : je.getElements()) {
					Node toadd = g.getNode(ele);
					if (!joinElements.containsKey(toadd))
						joinElements.put(toadd, new ArrayList<Node>());
					joinElements.get(toadd).add(n);
				}
			}
			
			if (element instanceof MeetElement) {
				MeetElement je = (MeetElement) element;
				for (Element ele : je.getElements()) {
					Node toadd = g.getNode(ele);
					if (!meetElements.containsKey(toadd))
						meetElements.put(toadd, new ArrayList<Node>());
					meetElements.get(toadd).add(n);
				}
			}
			
			if (element instanceof ConstructorApplication) {
				ConstructorApplication ce = (ConstructorApplication) element;
				for (Element ele : ce.getElements()) {
					Node toadd = g.getNode(ele);
					if (!consElements.containsKey(toadd))
						consElements.put(toadd, new ArrayList<Node>());
					consElements.get(toadd).add(n);
				}
			}
		}

		saturation();
	}
	
	public List<Edge> getPath(Node start, Node end, boolean verbose) {
		if (!initialized) {
			long startTime = System.currentTimeMillis();
			initialize();
			initialized = true;
			long endTime = System.currentTimeMillis();
			if (verbose)
				System.out.println("path_finding time: "
						+ (endTime - startTime));
		}

		return _getPath(start, end);
	}
	
	abstract protected List<Edge> _getPath(Node start, Node end);

	// the method used to generate all CFG nonterminals in a graph
	abstract public void saturation();
}
