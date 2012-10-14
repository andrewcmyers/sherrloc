package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.EdgeCondition;
import constraint.graph.IdEdge;
import constraint.graph.LeftEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

public class ShortestPathFinder extends CFLPathFinder {
	int MAX = 10000;
	List<Edge>[][] idPath;
	Map<EdgeCondition, List<Edge>>[][] leftPath;
	int[][] shortestID;
	Map<EdgeCondition, Integer>[][] shortestLeft;
	
	public ShortestPathFinder(ConstraintGraph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		idPath = new List[size][size];
		leftPath = new Map[size][size];
	}

	/**
     * This algorithm follows the CFL-Reachablity algorithm described in paper
     * interconvertibility of set constraints and context-free language reachability
     * by David Melski and Thomas Reps
     * The complexity is O (|\Sigma|^3 n^3), where \Sigma is grammar size, n is # of nodes
     * 
     * Here is the grammar we use:
     * id := left right | id id
     * left := left id
     * In order to find the (shortest) reduction path for error diagnostic, we use the
     * dynamic programming algorithm proposed by CHRIS BARRETT, RIKO JACOB, AND MADHAV MARATHE
     * The idea is that we are interested in the shortest path from two nodes, that can be
     * derived from the nonterminal "id" in grammar
     */
	public void saturation() {
		List<Node> allNodes = g.getAllNodes();
		int size = allNodes.size();
		List<ReductionEdge> alledges = getAllReductionEdges();
		
		shortestID = new int[size][size];
		shortestLeft = new Map[size][size];
		Set<EdgeCondition>[][] leftConditions = new Set[size][size];
		
		// Step 1: initialize graph, fix 10000 later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				int sIndex = g.getIndex(start);
				int eIndex = g.getIndex(end);
				
				idPath[sIndex][eIndex] = new ArrayList<Edge>();
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition,List<Edge>>();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestID[sIndex][eIndex] = MAX;
				leftConditions[sIndex][eIndex] = new HashSet<EdgeCondition>();
			}
		}
		
		for (Node n : allNodes) {
			shortestID[g.getIndex(n)][g.getIndex(n)] = 0;
		}
		
		for (Edge e : alledges) {
			int fIndex = g.getIndex(e.getFrom());
			int tIndex = g.getIndex(e.getTo());
			
			if (e instanceof IdEdge) {
				shortestID[fIndex][tIndex] = 1;
				idPath[fIndex][tIndex].addAll(((IdEdge) e).getEdges());
			}
			
			if (e instanceof LeftEdge) {
				LeftEdge le = (LeftEdge) e;
				shortestLeft[fIndex][tIndex].put(le.cons, 1);
				List<Edge> edges = new ArrayList<Edge>();
				edges.addAll(le.getEdges());
				leftPath[fIndex][tIndex].put(le.cons, edges);
				leftConditions[fIndex][tIndex].add(le.cons);
			}
		}

		PriorityQueue<ReductionEdge> queue = new PriorityQueue<ReductionEdge>(alledges);
		
		// use the priority queue as a working list, and update the table
		while (!queue.isEmpty()) {
			ReductionEdge edge = queue.poll();
			
			// when edge is the left part of reduction
			for (Node to : allNodes) {
				int sIndex = g.getIndex(edge.getFrom());
				int fIndex = g.getIndex(edge.getTo());
				int tIndex = g.getIndex(to);
				Node from = edge.getTo();

				// id = id id
				if (edge instanceof IdEdge) {
					if (shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestID[sIndex][tIndex]) {
						shortestID[sIndex][tIndex] = shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex];
						idPath[sIndex][tIndex].clear();
						idPath[sIndex][tIndex].addAll(idPath[sIndex][fIndex]);
						idPath[sIndex][tIndex].addAll(idPath[fIndex][tIndex]);
						queue.offer(new IdEdge(edge.getFrom(), to, idPath[sIndex][tIndex]));
//								System.out.println(edge.getFrom()+"-id-"+to);
					}
				}
	
				else if (edge instanceof LeftEdge) {
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						// left := left id
						if (!shortestLeft[sIndex][tIndex].containsKey(ec) ||
							shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex].get(ec)) {
							shortestLeft[sIndex][tIndex].put(ec, shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex]);
							if (leftPath[sIndex][tIndex].containsKey(ec))
								leftPath[sIndex][tIndex].get(ec).clear();
							List<Edge> edges = new ArrayList<Edge>();
							if (leftPath[sIndex][fIndex].get(ec) == null) {
								System.out.println(edge);
							}
							edges.addAll(leftPath[sIndex][fIndex].get(ec));
							edges.addAll(idPath[fIndex][tIndex]);
							leftPath[sIndex][tIndex].put(ec, edges);
							leftConditions[sIndex][tIndex].add(ec);
							queue.offer(new LeftEdge(ec, edge.getFrom(), to, edges));
//								System.out.println(edge.getFrom()+"-left-"+from+"-id-"+to+" implies "+edge.getFrom()+"-left-"+to);
						}
			
				// id = left right
						if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestID[sIndex][tIndex]) {
							// first check that left and right edges can be cancelled
							for (RightEdge e : getRightEdges(from, to)) {
								if (e!=null && ec.matches(e.cons)) {
									shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex].get(ec) + 1;
									idPath[sIndex][tIndex].clear();
									idPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex].get(ec));
									idPath[sIndex][tIndex].addAll(((RightEdge)e).getEdges());
									queue.offer(new IdEdge(edge.getFrom(), to, idPath[sIndex][tIndex]));
//									System.out.println(edge.getFrom() + "-id-" + to);
								}
							}
						}
					}
				}
			}
			
			// when edge is right part of reduction
			for (Node from : allNodes) {
				int sIndex = g.getIndex(from);
				int fIndex = g.getIndex(edge.getFrom());
				int tIndex = g.getIndex(edge.getTo());

				// id = id id
				if (edge instanceof IdEdge) {
					if (shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestID[sIndex][tIndex]) {
						shortestID[sIndex][tIndex] = shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex];
						idPath[sIndex][tIndex].clear();
						idPath[sIndex][tIndex].addAll(idPath[sIndex][fIndex]);
						idPath[sIndex][tIndex].addAll(idPath[fIndex][tIndex]);
						queue.offer(new IdEdge(from, edge.getTo(), idPath[sIndex][tIndex]));
//											System.out.println(from+"-id-"+edge.getTo());
					}
	
					// left := left id
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						if (!shortestLeft[sIndex][tIndex].containsKey(ec) ||
							shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex].get(ec)) {
							shortestLeft[sIndex][tIndex].put(ec, shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex]);
							if (leftPath[sIndex][tIndex].containsKey(ec))
								leftPath[sIndex][tIndex].get(ec).clear();
							List<Edge> edges = new ArrayList<Edge>();
							edges.addAll(leftPath[sIndex][fIndex].get(ec));
							edges.addAll(idPath[fIndex][tIndex]);
							leftPath[sIndex][tIndex].put(ec, edges);
							leftConditions[sIndex][tIndex].add(ec);
							queue.offer(new LeftEdge(ec, from, edge.getTo(), edges));
	//										System.out.println(start+"-left-"+from+"-id-"+to+" implies "+start+"-left-"+to);
						}
					}
				}
			
				// id = left right
				if (edge instanceof RightEdge) {
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestID[sIndex][tIndex]) {
							// first check that left and right edges can be cancelled
							if (ec.matches(((RightEdge)edge).cons)) {
								shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex].get(ec) + 1;
								idPath[sIndex][tIndex].clear();
								idPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex].get(ec));
								idPath[sIndex][tIndex].addAll(((RightEdge)edge).getEdges());
								queue.offer(new IdEdge(from, edge.getTo(), idPath[sIndex][tIndex]));
//												System.out.println(from + "-id-" + edge.getTo());
							}
						}
					}
				}
			}
		}
	}


	@Override
	protected List<Edge> _getPath(Node start, Node end) {
		int sIndex = g.getIndex(start);
		int eIndex = g.getIndex(end);
		
		if ( shortestID[sIndex][eIndex]!=MAX) {
			return idPath[sIndex][eIndex];
		}
		else 
			return null;
	}

}
