package constraint.graph.pathfinder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import util.FibonacciHeap;
import util.FibonacciHeapNode;
import constraint.ast.ComplexElement;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.EdgeCondition;
import constraint.graph.ElementNode;
import constraint.graph.EmptyEdge;
import constraint.graph.LeftEdge;
import constraint.graph.LeqEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;
import constraint.graph.Polarity;;

public class ShortestPathFinder extends CFLPathFinder {
	int MAX = 10000;
	FibonacciHeapNode<ReductionEdge>[][] idPath;
	Map<EdgeCondition, FibonacciHeapNode<ReductionEdge>>[][] leftPath;
	int[][] shortestLEQ;
	boolean CORRECTNESS_CHECK = false;
	Map<EdgeCondition, Integer>[][] shortestLeft;
	Environment env;
	
	public ShortestPathFinder(ConstraintGraph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		idPath = new FibonacciHeapNode[size][size];
		leftPath = new HashMap[size][size];
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
		
		shortestLEQ = new int[size][size];
		shortestLeft = new HashMap[size][size];
		
		// Step 1: initialize graph, fix MAX later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();
				
				idPath[sIndex][eIndex] = null;
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition,FibonacciHeapNode<ReductionEdge>>();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestLEQ[sIndex][eIndex] = MAX;
			}
		}
		
//		PriorityQueue<ReductionEdge> queue = new PriorityQueue<ReductionEdge>(10,
//		        new Comparator<ReductionEdge>() {
//	          		public int compare(ReductionEdge o1, ReductionEdge o2) {
//	          			return o1.getLength() - o2.getLength();
//	          		}
//				});
		FibonacciHeap<ReductionEdge> fh = new FibonacciHeap<ReductionEdge>();
		
		for (Node n : allNodes) {
			shortestLEQ[n.getIndex()][n.getIndex()] = 0;
			ReductionEdge e = new LeqEdge(n, n, EmptyEdge.getInstance(), EmptyEdge.getInstance());
			FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(e, e.getLength());
			idPath[n.getIndex()][n.getIndex()] = node;
		}
		
		for (Edge e : alledges) {
			int fIndex = e.getFrom().getIndex();
			int tIndex = e.getTo().getIndex();
			
			if (e instanceof LeqEdge) {
				shortestLEQ[fIndex][tIndex] = 1;
				FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>((LeqEdge)e, ((LeqEdge) e).getLength());
				idPath[fIndex][tIndex] = node;
				fh.insert(node, node.getKey());
			}
			
			if (e instanceof LeftEdge) {
				LeftEdge le = (LeftEdge) e;
				shortestLeft[fIndex][tIndex].put(le.cons, 1);
				FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(le, le.getLength());
				leftPath[fIndex][tIndex].put(le.cons, node);
				fh.insert(node, node.getKey());
			}
		}
		
		// use the priority queue as a working list, and update the table
		int current_length = 0;
		int count=1;
		while (!fh.isEmpty()) {	
			ReductionEdge edge = fh.removeMin().getData();
			if (edge instanceof LeqEdge)
				tryAddingBackEdges ((LeqEdge)edge, fh);

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
						if (shortestLEQ[leftmost.getIndex()][e.getTo().getIndex()]<MAX)
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
				if (shortestLEQ[edge.getFrom().getIndex()][edge.getTo().getIndex()] != edge.getEdges().size()) {
					System.err.println("Oooooooops, the id edge"+edge.getFrom() + " "+edge.getTo()+"is not the shortest");
					System.exit(0);
				}
			}
			if (CORRECTNESS_CHECK && edge instanceof LeftEdge) {
				EdgeCondition ec = ((LeftEdge)edge).getCondition();
				if (shortestLeft[edge.getFrom().getIndex()][edge.getTo().getIndex()].get(ec) 
						!= edge.getEdges().size()) {
					System.err.println("Oooooooops, the left edge" +edge.getFrom() + " "+edge.getTo()+ "is not the shortest");
					System.exit(0);
				}
			}
			current_length = edge.getEdges().size();
						
			// when edge is the left part of reduction
			for (Node to : allNodes) {
				int sIndex = edge.getFrom().getIndex();
				int fIndex = edge.getTo().getIndex();
				int tIndex = to.getIndex();
				Node from = edge.getTo();
				
//				if (sIndex==fIndex || sIndex==tIndex || fIndex==tIndex)
//					continue;

				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestLEQ[sIndex][fIndex] + shortestLEQ[fIndex][tIndex] < shortestLEQ[sIndex][tIndex]) {
						ReductionEdge newEdge = new LeqEdge(edge.getFrom(), to, edge, idPath[fIndex][tIndex].getData());
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (idPath[sIndex][tIndex]!=null)
							fh.delete(idPath[sIndex][tIndex]);
						FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newEdge, newEdge.getLength());
						fh.insert(node, node.getKey());
						idPath[sIndex][tIndex] = node;
						
						if (CORRECTNESS_CHECK /*&& shortestLEQ[sIndex][tIndex] < current_length*/)
							System.err.println("[LEFT] " + edge.getFrom()
									+ "-id-" + from + "-id-" + to + " implies "
									+ edge.getFrom() + "-id-" + to);
					}
				}
	
				else if (edge instanceof LeftEdge) {
					EdgeCondition ec = ((LeftEdge)edge).getCondition();
					
					if (ec.getPolarity()==Polarity.POS) {
						// left = left id
						if (shortestLEQ[fIndex][tIndex] < MAX) {
							if (!shortestLeft[sIndex][tIndex].containsKey(ec)
									|| shortestLeft[sIndex][fIndex].get(ec)
											+ shortestLEQ[fIndex][tIndex] < shortestLeft[sIndex][tIndex]
											.get(ec)) {

								ReductionEdge newedge = new LeftEdge(ec, edge
										.getFrom(), to, edge,
										idPath[fIndex][tIndex].getData());

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[fIndex][tIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									fh.delete(leftPath[sIndex][tIndex].get(ec));
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(
										newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								leftPath[sIndex][tIndex].put(ec, node);

								if (CORRECTNESS_CHECK
										/*&& shortestLeft[sIndex][tIndex].get(ec) < current_length*/)
									System.err.println("[LEFT] "
											+ edge.getFrom() + "-left-" + from
											+ "-id-" + to + " implies "
											+ edge.getFrom() + "-left-" + to);
							}
						}
					}
					else { // negative polarity, should use negative LEQ edges.
						// left = left id
						if (shortestLEQ[tIndex][fIndex] < MAX) {
							if (!shortestLeft[sIndex][tIndex].containsKey(ec)
									|| shortestLeft[sIndex][fIndex].get(ec) + shortestLEQ[tIndex][fIndex] < shortestLeft[sIndex][tIndex].get(ec)) {

								ReductionEdge newedge = new LeftEdge(ec, edge
										.getFrom(), to, edge,
										idPath[tIndex][fIndex].getData().getReverse());

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[tIndex][fIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									fh.delete(leftPath[sIndex][tIndex].get(ec));
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(
										newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								leftPath[sIndex][tIndex].put(ec, node);

								if (CORRECTNESS_CHECK
										/*&& shortestLeft[sIndex][tIndex].get(ec) < current_length*/)
									System.err.println("[LEFT] "
											+ edge.getFrom() + "-left-" + from
											+ "-id-" + to + " implies "
											+ edge.getFrom() + "-left-" + to);
							}
						}
					}

					// id = left right
					if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestLEQ[sIndex][tIndex]) {
						// first check that left and right edges can
						// canceled
						for (RightEdge e : getRightEdges(from, to)) {
							if (e != null && ec.matches(e.cons)) {
								ReductionEdge newedge = new LeqEdge(edge.getFrom(), to, edge, e);

								shortestLEQ[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (idPath[sIndex][tIndex] != null)
									fh.delete(idPath[sIndex][tIndex]);
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(
										newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								idPath[sIndex][tIndex] = node;

								if (CORRECTNESS_CHECK
										/*&& shortestLEQ[sIndex][tIndex] < current_length*/)
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
				int sIndex = from.getIndex();
				int fIndex = edge.getFrom().getIndex();
				int tIndex = edge.getTo().getIndex();
				
//				if (sIndex==fIndex || sIndex==tIndex || fIndex==tIndex)
//					continue;

				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestLEQ[sIndex][fIndex] + shortestLEQ[fIndex][tIndex] < shortestLEQ[sIndex][tIndex]) {
						ReductionEdge newedge = new LeqEdge(from, edge.getTo(),idPath[sIndex][fIndex].getData(), edge);
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (idPath[sIndex][tIndex]!=null)
							fh.delete(idPath[sIndex][tIndex]);
						FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newedge, newedge.getLength());
						fh.insert(node, node.getKey());
						idPath[sIndex][tIndex] = node;

						if (CORRECTNESS_CHECK /*&& shortestLEQ[sIndex][tIndex]<current_length*/)
							System.err.println("[RIGHT] " + from + "-id-"
									+ edge.getFrom() + "-id-" + edge.getTo()
									+ " implies " + from + "-id-"
									+ edge.getTo());
					}
	
					// left := left id
					// first, use id edge as POS LEQ edge
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						if (ec.getPolarity()==Polarity.POS) {
							if (!shortestLeft[sIndex][tIndex].containsKey(ec)
									|| shortestLeft[sIndex][fIndex].get(ec)
											+ shortestLEQ[fIndex][tIndex] < shortestLeft[sIndex][tIndex]
											.get(ec)) {

								ReductionEdge newedge = new LeftEdge(ec, from,
										edge.getTo(), leftPath[sIndex][fIndex]
												.get(ec).getData(), edge);

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[fIndex][tIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									fh.delete(leftPath[sIndex][tIndex].get(ec));
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(
										newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								leftPath[sIndex][tIndex].put(ec, node);

								if (CORRECTNESS_CHECK
										/*&& shortestLeft[sIndex][tIndex].get(ec) < current_length*/)
									System.err.println("[RIGHT] " + from
											+ "-left-" + edge.getFrom()
											+ "-id-" + edge.getTo()
											+ " implies " + from + "-left-"
											+ edge.getTo());
							}
						}
					}	
				}
			
				// id = left right
				if (edge instanceof RightEdge) {
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						// first check that left and right edges can be cancelled
						if (ec.matches(((RightEdge)edge).cons)) {
							if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestLEQ[sIndex][tIndex]) {
								ReductionEdge newedge = new LeqEdge(from, edge.getTo(), leftPath[sIndex][fIndex].get(ec).getData(), edge);

								shortestLEQ[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (idPath[sIndex][tIndex]!=null)
									fh.delete(idPath[sIndex][tIndex]);
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								idPath[sIndex][tIndex] = node;

								if (CORRECTNESS_CHECK /*&& shortestLEQ[sIndex][tIndex]<current_length*/)
									System.err.println("[RIGHT] " + from
											+ "-left-" + edge.getFrom()
											+ "-right-" + edge.getTo()
											+ " implies " + from + "-id-"
											+ edge.getTo());
							}
						}
					}
				}
				
				// try to use the reverse id edge as NEG LEQ edge
				if (edge instanceof LeqEdge) {
					sIndex = from.getIndex();
					fIndex = edge.getTo().getIndex();
					tIndex = edge.getFrom().getIndex();
					for (EdgeCondition ec : shortestLeft[sIndex][fIndex].keySet()) {
						if (ec.getPolarity() == Polarity.NEG) {
							if (!shortestLeft[sIndex][tIndex].containsKey(ec)
									|| shortestLeft[sIndex][fIndex].get(ec)
											+ shortestLEQ[tIndex][fIndex] < shortestLeft[sIndex][tIndex]
											.get(ec)) {

								ReductionEdge newedge = new LeftEdge(ec, from,
										edge.getFrom(), leftPath[sIndex][fIndex]
												.get(ec).getData(), edge.getReverse());

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[tIndex][fIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									fh.delete(leftPath[sIndex][tIndex].get(ec));
								FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(
										newedge, newedge.getLength());
								fh.insert(node, node.getKey());
								leftPath[sIndex][tIndex].put(ec, node);

								if (CORRECTNESS_CHECK
										/*&& shortestLeft[sIndex][tIndex].get(ec) < current_length*/)
									System.err.println("[RIGHT] " + from
											+ "-left-" + edge.getTo()
											+ "-id-" + edge.getFrom()
											+ " implies " + from + "-left-"
											+ edge.getFrom());
							}
						}
					}
				}
			}
			
			// consistancy check
//			if (CORRECTNESS_CHECK) {
//				for (Node start : allNodes) {
//					for (Node end : allNodes) {
//						int startIndex = start.getIndex();
//						int endIndex = end.getIndex();
//						for (EdgeCondition ec : shortestLeft[startIndex][endIndex]
//								.keySet()) {
//							if (shortestLeft[startIndex][endIndex].get(ec) != leftPath[startIndex][endIndex]
//									.get(ec).getEdges().size()) {
//								System.err.println("Ooooooops" + start + " to " + end);
//								System.exit(1);
//							}
//						}
//						if (idPath[startIndex][endIndex] != null
//								&& shortestID[startIndex][endIndex] != idPath[startIndex][endIndex]
//										.getEdges().size()) {
//							System.err.println("Ooooooops" + start + " to " + end);
//							System.exit(1);
//						}
//					}
//				}
//			}
		}
	}
	
	// given a newly discovered IdEdge, this function tries to identify back edges
	void tryAddingBackEdges (LeqEdge edge, FibonacciHeap<ReductionEdge> fh) {
		Node from = edge.getFrom();
		Node to = edge.getTo();
		
		// if to is an element of a meet label, check if some node flows into all components
		for (Node meetnode : meetElements.get(to)) {
			MeetElement me = (MeetElement) ((ElementNode)meetnode).getElement();
			Node candidate = from;
			int candIndex = candidate.getIndex();
			int meetIndex = meetnode.getIndex();
			boolean success = false;
			Edge redEdge=EmptyEdge.getInstance();
				
			if (idPath[candIndex][meetIndex]!=null)
				continue;
			for (Element e : me.getElements()) {
				int eleIndex = g.getNode(e).getIndex();
				if (idPath[candIndex][eleIndex]!=null) {
					redEdge = new LeqEdge(candidate, meetnode, idPath[candIndex][eleIndex].getData(), redEdge);
					success = true;
					continue;
				}
				else {
					success = false;
					break;
				}	
			}
			if (success) {
				ReductionEdge newedge = new LeqEdge(candidate, meetnode, redEdge, EmptyEdge.getInstance());
//				// this number is a little ad hoc
				shortestLEQ[candIndex][meetIndex] = newedge.getLength();
				FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newedge, newedge.getLength());
				fh.insert(node, node.getKey());
				idPath[candIndex][meetIndex] = node;
			}
		}
		
		// if from is an element of a join label, check if all components flows into some node
		for (Node joinnode : joinElements.get(from)) {
			JoinElement je = (JoinElement) ((ElementNode)joinnode).getElement();
			Node candidate = to;
			int candIndex = candidate.getIndex();
			int joinIndex = joinnode.getIndex();
			boolean success = true;
			Edge redEdge = EmptyEdge.getInstance();
				
			if (idPath[joinIndex][candIndex]!=null)
					continue;
			for (Element e : je.getElements()) {
				int eleIndex = g.getNode(e).getIndex();
				if (idPath[eleIndex][candIndex]!=null) {
					redEdge = new LeqEdge(joinnode, candidate, idPath[eleIndex][candIndex].getData(), redEdge);
					continue;
				}
				else {
					success = false;
					break;
				}	
			}
			if (success) {
				ReductionEdge newedge = new LeqEdge(joinnode, candidate, redEdge, EmptyEdge.getInstance());
				// this number is a little ad hoc
				shortestLEQ[joinIndex][candIndex] = newedge.getLength();
				FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newedge, newedge.getLength());
				fh.insert(node, node.getKey());
				idPath[joinIndex][candIndex] = node;
			}
		}
		
		// if from and to belongs to some constructors, check if this new link enables a leq relation on the
		// constructors
		for (Node cnFrom : consElements.get(from)) {
			for (Node cnTo : consElements.get(to)) {
				ComplexElement ce1 = (ComplexElement) ((ElementNode)cnFrom).getElement();  // make sure this is "ce1", not the swapped one when the constructor is contravariant
				
				if (idPath[cnFrom.getIndex()][cnTo.getIndex()]!=null)
					continue;
				ComplexElement ce2 = (ComplexElement) ((ElementNode)cnTo).getElement();
				
				if (ce1.getCons().equals(ce2.getCons())) {
					if (ce1.getCons().isContraVariant()) {
						ComplexElement temp = ce1;
						ce1 = ce2;
						ce2 = temp;
					}
					
					// check if all elements flows into another constructor
					boolean success = true;
					for (int i=0; i<ce1.getCons().getArity(); i++) {
						Element e1 = ce1.getElements().get(i);
						Element e2 = ce2.getElements().get(i);
						if (idPath[g.getNode(e1).getIndex()][g.getNode(e2).getIndex()]==null) {
							success = false;
							break;
						}
					}
					if (success) {						
//						System.out.println("Adding edge from "+cnFrom + " to " + cnTo);
//						System.out.println("Because of edge "+edge.getFrom() + " to " + edge.getTo());
						ReductionEdge newedge = new LeqEdge(cnFrom, cnTo, edge, EmptyEdge.getInstance());
						// this number is a little ad hoc
						shortestLEQ[cnFrom.getIndex()][cnTo.getIndex()] = newedge.getLength();
						FibonacciHeapNode<ReductionEdge> node = new FibonacciHeapNode<ReductionEdge>(newedge, newedge.getLength());
						fh.insert(node, node.getKey());
						idPath[cnFrom.getIndex()][cnTo.getIndex()] = node;
					}
				}
			}
		}
	}


	@Override
	protected List<Edge> _getPath(Node start, Node end) {
		int sIndex = start.getIndex();
		int eIndex = end.getIndex();
		
		if ( idPath[sIndex][eIndex]!=null) {
			return idPath[sIndex][eIndex].getData().getEdges();
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
