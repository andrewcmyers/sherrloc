package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

import constraint.graph.Edge;
import constraint.graph.EdgeCondition;
import constraint.graph.Graph;
import constraint.graph.IdEdge;
import constraint.graph.LeftEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

public class ShortestPathFinder extends CFLPathFinder {
	int MAX = 10000;
	List<Edge>[][] idPath;
	List<Edge>[][] leftPath;
	int[][] shortestID;
	int[][] shortestLeft;
	
	public ShortestPathFinder(Graph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		idPath = new List[size][size];
		leftPath = new List[size][size];
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
		shortestLeft = new int[size][size];
		EdgeCondition[][] leftCondition = new EdgeCondition[size][size];
		
		// Step 1: initialize graph, fix 10000 later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				int sIndex = g.getIndex(start);
				int eIndex = g.getIndex(end);
				
				idPath[sIndex][eIndex] = new ArrayList<Edge>();
				leftPath[sIndex][eIndex] = new ArrayList<Edge>();
				shortestID[sIndex][eIndex] = MAX;
				shortestLeft[sIndex][eIndex] = MAX;
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
				shortestLeft[fIndex][tIndex] = 1;
				leftPath[fIndex][tIndex].addAll(((LeftEdge) e).getEdges());
				leftCondition[fIndex][tIndex] = ((LeftEdge)e).cons;
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
					// left := left id
					if (shortestLeft[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex]) {
						shortestLeft[sIndex][tIndex] = shortestLeft[sIndex][fIndex] + shortestID[fIndex][tIndex];
						leftPath[sIndex][tIndex].clear();
						leftPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex]);
						leftPath[sIndex][tIndex].addAll(idPath[fIndex][tIndex]);
						leftCondition[sIndex][tIndex] = leftCondition[sIndex][fIndex];
						queue.offer(new LeftEdge(leftCondition[sIndex][tIndex], edge.getFrom(), to, leftPath[sIndex][tIndex]));
	//							System.out.println(start+"-left-"+from+"-id-"+to+" implies "+start+"-left-"+to);
					}
			
				// id = left right
					else if (shortestLeft[sIndex][fIndex] + 1 < shortestID[sIndex][tIndex]) {
						// first check that left and right edges can be cancelled
						RightEdge e = getRightEdge(from, to);
						if (e!=null && leftCondition[sIndex][fIndex].matches(e.cons)) {
							shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex] + 1;
							idPath[sIndex][tIndex].clear();
							idPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex]);
							idPath[sIndex][tIndex].addAll(((RightEdge)e).getEdges());
							queue.offer(new IdEdge(edge.getFrom(), to, idPath[sIndex][tIndex]));
//									System.out.println(edge.getFrom() + "-id-" + to);
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
					if (shortestLeft[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex]) {
						shortestLeft[sIndex][tIndex] = shortestLeft[sIndex][fIndex] + shortestID[fIndex][tIndex];
						leftPath[sIndex][tIndex].clear();
						leftPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex]);
						leftPath[sIndex][tIndex].addAll(idPath[fIndex][tIndex]);
						leftCondition[sIndex][tIndex] = leftCondition[sIndex][fIndex];
						queue.offer(new LeftEdge(leftCondition[sIndex][tIndex], from, edge.getTo(), leftPath[sIndex][tIndex]));
	//										System.out.println(start+"-left-"+from+"-id-"+to+" implies "+start+"-left-"+to);
					}
				}
			
				// id = left right
				if (edge instanceof RightEdge) {
					if (shortestLeft[sIndex][fIndex] + 1 < shortestID[sIndex][tIndex]) {
						// first check that left and right edges can be cancelled
						if (leftCondition[sIndex][fIndex].matches(((RightEdge)edge).cons)) {
							shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex] + 1;
							idPath[sIndex][tIndex].clear();
							idPath[sIndex][tIndex].addAll(leftPath[sIndex][fIndex]);
							idPath[sIndex][tIndex].addAll(((RightEdge)edge).getEdges());
							queue.offer(new IdEdge(from, edge.getTo(), idPath[sIndex][tIndex]));
//												System.out.println(from + "-id-" + edge.getTo());
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
