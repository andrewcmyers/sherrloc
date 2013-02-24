package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.EdgeCondition;
import constraint.graph.ElementNode;
import constraint.graph.LeqEdge;
import constraint.graph.LeftEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

public class ShortestPathFinder extends CFLPathFinder {
	int MAX = 10000;
	ReductionEdge[][] idPath;
	Map<EdgeCondition, ReductionEdge>[][] leftPath;
	int[][] shortestID;
	boolean CORRECTNESS_CHECK = false;
	Map<EdgeCondition, Integer>[][] shortestLeft;
	Environment env;
	
	public ShortestPathFinder(ConstraintGraph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		idPath = new ReductionEdge[size][size];
		leftPath = new Map[size][size];
		env = new Environment();
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
		
		// Step 1: initialize graph, fix MAX later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				int sIndex = g.getIndex(start);
				int eIndex = g.getIndex(end);
				
				idPath[sIndex][eIndex] = null;
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition,ReductionEdge>();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestID[sIndex][eIndex] = MAX;
			}
		}
		
		PriorityQueue<ReductionEdge> queue = new PriorityQueue<ReductionEdge>(10,
		        new Comparator<ReductionEdge>() {
	          		public int compare(ReductionEdge o1, ReductionEdge o2) {
	          			return o1.getLength() - o2.getLength();
	          		}
				});
		
		for (Node n : allNodes) {
			shortestID[g.getIndex(n)][g.getIndex(n)] = 0;
			idPath[g.getIndex(n)][g.getIndex(n)] = new LeqEdge(n, n, null, null);
		}
		
		for (Edge e : alledges) {
			int fIndex = g.getIndex(e.getFrom());
			int tIndex = g.getIndex(e.getTo());
			
			if (e instanceof LeqEdge) {
				shortestID[fIndex][tIndex] = 1;
				idPath[fIndex][tIndex] = (ReductionEdge)e;
				queue.offer((ReductionEdge)e);
			}
			
			if (e instanceof LeftEdge) {
				LeftEdge le = (LeftEdge) e;
				shortestLeft[fIndex][tIndex].put(le.cons, 1);
				leftPath[fIndex][tIndex].put(le.cons, le);
				queue.offer((ReductionEdge)e);
			}
		}
		
		// use the priority queue as a working list, and update the table
		int current_length = 0;
		while (!queue.isEmpty()) {
			
			ReductionEdge edge = queue.poll();
			if (edge instanceof LeqEdge)
				tryAddingBackEdges ((LeqEdge)edge, queue);

			if (CORRECTNESS_CHECK && edge instanceof LeqEdge) {
				System.out.println(edge.getLength());
				List<Edge> edges = edge.getEdges();
				if (edges.size()!=0) {
					System.out.println( "\n----Start of one path----\n");
					ElementNode leftmost = (ElementNode) edges.get(0).getFrom();
					System.out.println(leftmost.getName()+"\n");
					for (int k = 0; k < edges.size(); k++) {
						Edge e = edges.get(k);
//					ret += "--> (" + (edge.toString()) + ")\n";
						if (shortestID[g.getIndex(leftmost)][g.getIndex(e.getTo())]<MAX)
							System.out.println (((ElementNode)e.getTo()).getName()+"\n");
				}
				System.out.println( "----End of one path----\n");
				}
			}

			if (CORRECTNESS_CHECK && current_length>edge.getEdges().size()) {
				System.err.println("Oooooooops, got a smaller edge");
				System.exit(0);
			}
			if (CORRECTNESS_CHECK && edge instanceof LeqEdge) {
				if (shortestID[g.getIndex(edge.getFrom())][g.getIndex(edge.getTo())] != edge.getEdges().size()) {
					System.err.println("Oooooooops, the id edge"+edge.getFrom() + " "+edge.getTo()+"is not the shortest");
					System.exit(0);
				}
			}
			if (CORRECTNESS_CHECK && edge instanceof LeftEdge) {
				EdgeCondition ec = ((LeftEdge)edge).getCondition();
				if (shortestLeft[g.getIndex(edge.getFrom())][g.getIndex(edge.getTo())].get(ec) 
						!= edge.getEdges().size()) {
					System.err.println("Oooooooops, the left edge" +edge.getFrom() + " "+edge.getTo()+ "is not the shortest");
					System.exit(0);
				}
			}
			current_length = edge.getEdges().size();
						
			// when edge is the left part of reduction
			for (Node to : allNodes) {
				int sIndex = g.getIndex(edge.getFrom());
				int fIndex = g.getIndex(edge.getTo());
				int tIndex = g.getIndex(to);
				Node from = edge.getTo();
				
				if (sIndex==fIndex || sIndex==tIndex || fIndex==tIndex)
					continue;

				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestID[sIndex][tIndex]) {
						ReductionEdge newEdge = new LeqEdge(edge.getFrom(), to, edge, idPath[fIndex][tIndex]);
						
						shortestID[sIndex][tIndex] = shortestID[sIndex][fIndex]
								+ shortestID[fIndex][tIndex];
						if (idPath[sIndex][tIndex] != null)
							queue.remove(idPath[sIndex][tIndex]);
						idPath[sIndex][tIndex] = newEdge;
						queue.offer(newEdge);
						
						if (CORRECTNESS_CHECK && shortestID[sIndex][tIndex] < current_length)
							System.err.println("[LEFT] " + edge.getFrom()
									+ "-id-" + from + "-id-" + to + " implies "
									+ edge.getFrom() + "-id-" + to);
					}
				}
	
				else if (edge instanceof LeftEdge) {
					EdgeCondition ec = ((LeftEdge)edge).getCondition();

					// left = left id
					if (shortestID[fIndex][tIndex] < MAX) {
						if (!shortestLeft[sIndex][tIndex].containsKey(ec) || 
								shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex].get(ec)) {
							
							ReductionEdge newedge = new LeftEdge(ec, edge.getFrom(), to, edge, idPath[fIndex][tIndex]);
							
							shortestLeft[sIndex][tIndex].put(ec,
									shortestLeft[sIndex][fIndex].get(ec)
											+ shortestID[fIndex][tIndex]);
							if (leftPath[sIndex][tIndex].containsKey(ec))
								queue.remove(leftPath[sIndex][tIndex].get(ec));
							queue.offer(newedge);

							if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec) < current_length)
								System.err.println("[LEFT] " + edge.getFrom()
										+ "-left-" + from + "-id-" + to
										+ " implies " + edge.getFrom()
										+ "-left-" + to);
						}
					}

					// id = left right
					if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestID[sIndex][tIndex]) {
						// first check that left and right edges can canceled
						for (RightEdge e : getRightEdges(from, to)) {
							if (e != null && ec.matches(e.cons)) {
								ReductionEdge newedge = new LeqEdge(edge.getFrom(), to, edge, e);
								
								shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex].get(ec) + 1;
								if (idPath[sIndex][tIndex] != null)
									queue.remove(idPath[sIndex][tIndex]);
								queue.offer(newedge);
								idPath[sIndex][tIndex] = newedge;
								
								if (CORRECTNESS_CHECK && shortestID[sIndex][tIndex] < current_length)
									System.err.println("[LEFT] "
											+ edge.getFrom() + "-left-" + from
											+ "-right-" + to + " implies "
											+ edge.getFrom() + "-id-" + to);
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
				
				if (sIndex==fIndex || sIndex==tIndex || fIndex==tIndex)
					continue;

				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestID[sIndex][fIndex] + shortestID[fIndex][tIndex] < shortestID[sIndex][tIndex]) {
						ReductionEdge newedge = new LeqEdge(from, edge.getTo(),idPath[sIndex][fIndex], edge);
						
						shortestID[sIndex][tIndex] = shortestID[sIndex][fIndex]
								+ shortestID[fIndex][tIndex];
						if (idPath[sIndex][tIndex] != null)
							queue.remove(idPath[sIndex][tIndex]);
						queue.offer(newedge);
						idPath[sIndex][tIndex] = newedge;

						if (CORRECTNESS_CHECK && shortestID[sIndex][tIndex]<current_length)
							System.err.println("[RIGHT] " + from + "-id-"
									+ edge.getFrom() + "-id-" + edge.getTo()
									+ " implies " + from + "-id-"
									+ edge.getTo());
					}
	
					// left := left id
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						if (!shortestLeft[sIndex][tIndex].containsKey(ec) ||
							shortestLeft[sIndex][fIndex].get(ec) + shortestID[fIndex][tIndex] < shortestLeft[sIndex][tIndex].get(ec)) {
				
							ReductionEdge newedge = new LeftEdge(ec, from, edge.getTo(), leftPath[sIndex][fIndex].get(ec), edge);

							shortestLeft[sIndex][tIndex].put(ec,
									shortestLeft[sIndex][fIndex].get(ec)
											+ shortestID[fIndex][tIndex]);
							if (leftPath[sIndex][tIndex].containsKey(ec))
								queue.remove(leftPath[sIndex][tIndex].get(ec));
							queue.offer(newedge);
							leftPath[sIndex][tIndex].put(ec, newedge);
							
							if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec)<current_length)
								System.err.println("[RIGHT] " + from + "-left-"
										+ edge.getFrom() + "-id-"
										+ edge.getTo() + " implies " + from
										+ "-left-" + edge.getTo());
						}
					}
				}
			
				// id = left right
				if (edge instanceof RightEdge) {
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						// first check that left and right edges can be cancelled
						if (ec.matches(((RightEdge)edge).cons)) {
							if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestID[sIndex][tIndex]) {
								ReductionEdge newedge = new LeqEdge(from, edge.getTo(), leftPath[sIndex][fIndex].get(ec), edge);

								shortestID[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (idPath[sIndex][tIndex] != null)
									queue.remove(idPath[sIndex][tIndex]);
								queue.offer(newedge);
								idPath[sIndex][tIndex] = newedge;

								if (CORRECTNESS_CHECK && shortestID[sIndex][tIndex]<current_length)
									System.err.println("[RIGHT] " + from
											+ "-left-" + edge.getFrom()
											+ "-right-" + edge.getTo()
											+ " implies " + from + "-id-"
											+ edge.getTo());
							}
						}
					}
				}
			}
			
			// consistancy check
			if (CORRECTNESS_CHECK) {
				for (Node start : allNodes) {
					for (Node end : allNodes) {
						int startIndex = g.getIndex(start);
						int endIndex = g.getIndex(end);
						for (EdgeCondition ec : shortestLeft[startIndex][endIndex]
								.keySet()) {
							if (shortestLeft[startIndex][endIndex].get(ec) != leftPath[startIndex][endIndex]
									.get(ec).getEdges().size()) {
								System.err.println("Ooooooops" + start + " to " + end);
								System.exit(1);
							}
						}
						if (idPath[startIndex][endIndex] != null
								&& shortestID[startIndex][endIndex] != idPath[startIndex][endIndex]
										.getEdges().size()) {
							System.err.println("Ooooooops" + start + " to " + end);
							System.exit(1);
						}
					}
				}
			}
		}
	}
	
	// given a newly discovered IdEdge, this function tries to identify back edges
	void tryAddingBackEdges (LeqEdge edge, PriorityQueue<ReductionEdge> queue) {
		Node from = edge.getFrom();
		Node to = edge.getTo();
		
		// if to is an element of a meet label, check if some node flows into all components
		for (Node meetnode : meetElements.get(to)) {
			MeetElement me = (MeetElement) ((ElementNode)meetnode).getElement();
			Node candidate = from;
			int candIndex = g.getIndex(candidate);
			int meetIndex = g.getIndex(meetnode);
			boolean success = true;
			ReductionEdge redEdge=null;
				
			if (idPath[candIndex][meetIndex]!=null)
				continue;
			for (Element e : me.getElements()) {
				int eleIndex = g.getIndex(g.getNode(e));
				if (idPath[candIndex][eleIndex]!=null) {
					redEdge = new ReductionEdge(candidate, meetnode, idPath[candIndex][eleIndex], redEdge);
					continue;
				}
				else {
					success = false;
					break;
				}	
			}
			if (success) {
				ReductionEdge newedge = new LeqEdge(candidate, meetnode, redEdge, null);
				// this number is a little ad hoc
				shortestID[candIndex][meetIndex] = edge.getLength() + 1;
				queue.offer(newedge);
				idPath[candIndex][meetIndex] = newedge;
			}
		}
		
		// if from is an element of a join label, check if all components flows into some node
		for (Node joinnode : joinElements.get(from)) {
			JoinElement je = (JoinElement) ((ElementNode)joinnode).getElement();
			Node candidate = to;
			int candIndex = g.getIndex(candidate);
			int joinIndex = g.getIndex(joinnode);
			boolean success = true;
			ReductionEdge redEdge=null;
//			List<Edge> list = new ArrayList<Edge>();
				
			if (idPath[joinIndex][candIndex]!=null)
					continue;
			for (Element e : je.getElements()) {
				int eleIndex = g.getIndex(g.getNode(e));
				if (idPath[g.getIndex(g.getNode(e))][candIndex]!=null) {
					redEdge = new ReductionEdge(candidate, joinnode, idPath[eleIndex][candIndex], redEdge);
//					list.addAll(idPath[g.getIndex(g.getNode(e))][candIndex].getEdges());
					continue;
				}
				else {
					success = false;
					break;
				}	
			}
			if (success) {
				ReductionEdge newedge = new LeqEdge(joinnode, candidate, redEdge, null);
				// this number is a little ad hoc
				shortestID[joinIndex][candIndex] = edge.getLength() + 1;
				queue.offer(newedge);
				idPath[joinIndex][candIndex] = newedge;
			}
		}
	}


	@Override
	protected List<Edge> _getPath(Node start, Node end) {
		int sIndex = g.getIndex(start);
		int eIndex = g.getIndex(end);
		
		if ( idPath[sIndex][eIndex]!=null) {
			return idPath[sIndex][eIndex].getEdges();
		}
		else 
			return null;
	}

	// test if the leq relation can be derived from the graph
//	public boolean _hasPath (Element e1, Element e2) {
//		if (g.hasElement(e1) && g.hasElement(e2))
//			return _getPath(g.getNode(e1), g.getNode(e1))!=null;
//		// else, 
//		ElementNode node = g.getNode(e);
//		// update 
//	}
}
