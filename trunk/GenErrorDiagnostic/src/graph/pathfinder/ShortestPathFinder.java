package graph.pathfinder;

import graph.CompEdge;
import graph.ConstraintGraph;
import graph.Edge;
import graph.EdgeCondition;
import graph.EmptyEdge;
import graph.LeftEdge;
import graph.LeqEdge;
import graph.Node;
import graph.Polarity;
import graph.ReductionEdge;
import graph.RightEdge;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

import constraint.ast.ComplexElement;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Variable;

public class ShortestPathFinder extends CFLPathFinder {
	int MAX = 10000;
	ReductionEdge[][] idPath;
	Map<EdgeCondition, ReductionEdge>[][] leftPath;
	int[][] shortestLEQ;
	boolean CORRECTNESS_CHECK = false;
	Map<EdgeCondition, Integer>[][] shortestLeft;
	Environment env;
	
	public ShortestPathFinder(ConstraintGraph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		idPath = new ReductionEdge[size][size];
		leftPath = new HashMap[size][size];
		env = new Environment();
	}
	
	/**
	 * Finding the (shortest) reduction path for error diagnosis is an instance
	 * of the context-free-language-reachability problem with the following grammar:
	 * <p>
	 * id := left right | id id 
	 * left := left id
	 * <p>
	 * We use the dynamic programming algorithm proposed by Chris Barrett, Riko
	 * Jacob and Madhav Marathe. More details can be found in their paper 
	 * "Formal-language-constrained path problems". One difference is that we also
	 * handle "meet" and "join" when id edges are inferred
	 * 
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
				leftPath[sIndex][eIndex] = new HashMap<EdgeCondition,ReductionEdge>();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestLEQ[sIndex][eIndex] = MAX;
			}
		}
		
		PriorityQueue<ReductionEdge> queue = new PriorityQueue<ReductionEdge>(
				500, new Comparator<ReductionEdge>() {
					public int compare(ReductionEdge o1, ReductionEdge o2) {
						return o1.size - o2.size;
					}
				});

		
		for (Node n : allNodes) {
			shortestLEQ[n.getIndex()][n.getIndex()] = 0;
			ReductionEdge e = new LeqEdge(n, n, EmptyEdge.getInstance(), EmptyEdge.getInstance());
			idPath[n.getIndex()][n.getIndex()] = e;
		}
		
		for (ReductionEdge e : alledges) {
			int fIndex = e.getFrom().getIndex();
			int tIndex = e.getTo().getIndex();
			
			if (e instanceof LeqEdge) {
				shortestLEQ[fIndex][tIndex] = 1;
				idPath[fIndex][tIndex] = e;
				queue.offer(e);
			}
			
			if (e instanceof LeftEdge) {
				LeftEdge le = (LeftEdge) e;
				shortestLeft[fIndex][tIndex].put(le.cons, 1);
				leftPath[fIndex][tIndex].put(le.cons, le);
				queue.offer(e);
			}
		}
		
		// use a priority queue as a working list, and update the table
		int current_length = 0;
		while (!queue.isEmpty()) {	
			ReductionEdge edge = queue.poll();

			if (edge instanceof LeqEdge)
				tryAddingBackEdges ((LeqEdge)edge, queue);

			// the following code is activated for debugging only
			if (CORRECTNESS_CHECK && current_length>edge.getEdges().size()) {
				System.err.println("Error: got a smaller edge");
				System.exit(0);
			}
			if (CORRECTNESS_CHECK && edge instanceof LeqEdge) {
				if (shortestLEQ[edge.getFrom().getIndex()][edge.getTo().getIndex()] != edge.getEdges().size()) {
					System.err.println("Error: the id edge"+edge.getFrom() + " "+edge.getTo()+"is not the shortest");
					System.exit(0);
				}
			}
			if (CORRECTNESS_CHECK && edge instanceof LeftEdge) {
				EdgeCondition ec = ((LeftEdge)edge).getCondition();
				if (shortestLeft[edge.getFrom().getIndex()][edge.getTo().getIndex()].get(ec) 
						!= edge.getEdges().size()) {
					System.err.println("Error: the left edge" +edge.getFrom() + " "+edge.getTo()+ "is not the shortest");
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
				
				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestLEQ[sIndex][fIndex] + shortestLEQ[fIndex][tIndex] < shortestLEQ[sIndex][tIndex]) {
						ReductionEdge newedge = new LeqEdge(edge.getFrom(), to, edge, idPath[fIndex][tIndex]);
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (idPath[sIndex][tIndex]!=null)
							queue.remove(newedge);
						queue.offer(newedge);
						idPath[sIndex][tIndex] = newedge;
						
						if (CORRECTNESS_CHECK && shortestLEQ[sIndex][tIndex] < current_length)
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
										idPath[fIndex][tIndex]);

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[fIndex][tIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									queue.remove(leftPath[sIndex][tIndex].get(ec));
								queue.offer(newedge);;
								leftPath[sIndex][tIndex].put(ec, newedge);

								if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec) < current_length)
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
										idPath[tIndex][fIndex].getReverse());

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[tIndex][fIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									queue.remove(leftPath[sIndex][tIndex].get(ec));
								queue.offer(newedge);
								leftPath[sIndex][tIndex].put(ec, newedge);

								if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec) < current_length)
									System.err.println("[LEFT] "
											+ edge.getFrom() + "-left(neg)-" + from
											+ "-id(neg)-" + to + " implies "
											+ edge.getFrom() + "-left(neg)-" + to);
							}
						}
					}

					// id = left right
					if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestLEQ[sIndex][tIndex]) {
						// first check if left and right edges can be canceled
						for (RightEdge e : getRightEdges(from, to)) {
							if (e != null && ec.matches(e.cons)) {
								ReductionEdge newedge = new LeqEdge(edge.getFrom(), to, edge, e);

								shortestLEQ[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (idPath[sIndex][tIndex] != null)
									queue.remove(idPath[sIndex][tIndex]);
								queue.offer(newedge);
								idPath[sIndex][tIndex] = newedge;

								if (CORRECTNESS_CHECK && shortestLEQ[sIndex][tIndex] < current_length)
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
				
				// id = id id
				if (edge instanceof LeqEdge) {
					if (shortestLEQ[sIndex][fIndex] + shortestLEQ[fIndex][tIndex] < shortestLEQ[sIndex][tIndex]) {
						ReductionEdge newedge = new LeqEdge(from, edge.getTo(),idPath[sIndex][fIndex], edge);
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (idPath[sIndex][tIndex]!=null)
							queue.remove(idPath[sIndex][tIndex]);
						queue.offer(newedge);
						idPath[sIndex][tIndex] = newedge;

						if (CORRECTNESS_CHECK && shortestLEQ[sIndex][tIndex]<current_length)
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
												.get(ec), edge);

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[fIndex][tIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									queue.remove(leftPath[sIndex][tIndex].get(ec));
								queue.offer(newedge);
								leftPath[sIndex][tIndex].put(ec, newedge);

								if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec) < current_length)
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
						// first check that left and right edges can be canceled
						if (ec.matches(((RightEdge)edge).cons)) {
							if (shortestLeft[sIndex][fIndex].get(ec) + 1 < shortestLEQ[sIndex][tIndex]) {
								ReductionEdge newedge = new LeqEdge(from, edge.getTo(), leftPath[sIndex][fIndex].get(ec), edge);

								shortestLEQ[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (idPath[sIndex][tIndex]!=null)
									queue.remove(idPath[sIndex][tIndex]);
								queue.offer(newedge);
								idPath[sIndex][tIndex] = newedge;

								if (CORRECTNESS_CHECK && shortestLEQ[sIndex][tIndex]<current_length)
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
												.get(ec), edge.getReverse());

								shortestLeft[sIndex][tIndex].put(ec,
										shortestLeft[sIndex][fIndex].get(ec)
												+ shortestLEQ[tIndex][fIndex]);
								if (leftPath[sIndex][tIndex].containsKey(ec))
									queue.remove(leftPath[sIndex][tIndex].get(ec));
								queue.offer(newedge);
								leftPath[sIndex][tIndex].put(ec, newedge);

								if (CORRECTNESS_CHECK && shortestLeft[sIndex][tIndex].get(ec) < current_length)
									System.err.println("[RIGHT] " + from
											+ "-left(neg)-" + edge.getTo()
											+ "-id(neg)-" + edge.getFrom()
											+ " implies " + from + "-left-"
											+ edge.getFrom());
							}
						}
					}
				}
			}
			
			// consistency check
			if (CORRECTNESS_CHECK) {
				for (Node start : allNodes) {
					for (Node end : allNodes) {
						int startIndex = start.getIndex();
						int endIndex = end.getIndex();
						for (EdgeCondition ec : shortestLeft[startIndex][endIndex]
								.keySet()) {
							if (shortestLeft[startIndex][endIndex].get(ec) != leftPath[startIndex][endIndex]
									.get(ec).getEdges().size()) {
								System.err.println("Error: shortest left-path length is inconsistent from " + start + " to " + end);
								System.exit(1);
							}
						}
						if (idPath[startIndex][endIndex] != null
								&& shortestLEQ[startIndex][endIndex] != idPath[startIndex][endIndex]
										.getEdges().size()) {
							System.err.println("Error: shortest id-path length is inconsistent from " + start + " to " + end);
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
			MeetElement me = (MeetElement) meetnode.getElement();
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
					redEdge = new LeqEdge(candidate, meetnode, idPath[candIndex][eleIndex], redEdge);
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
				queue.offer(newedge);
				idPath[candIndex][meetIndex] = newedge;
			}
		}
		
		// if from is an element of a join label, check if all components flows into some node
		for (Node joinnode : joinElements.get(from)) {
			JoinElement je = (JoinElement) joinnode.getElement();
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
					redEdge = new LeqEdge(joinnode, candidate, idPath[eleIndex][candIndex], redEdge);
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
				queue.offer(newedge);
				idPath[joinIndex][candIndex] = newedge;
			}
		}
		
		// if from and to belongs to some constructors, check if this new link enables a leq relation on the
		// constructors
		for (Node cnFrom : consElements.get(from)) {
			for (Node cnTo : consElements.get(to)) {				
				ComplexElement ce1 = (ComplexElement) cnFrom.getElement();  // make sure this is "ce1", not the swapped one when the constructor is contravariant
				ComplexElement ce2 = (ComplexElement) cnTo.getElement();
				
				if (!ce1.getCons().isContraVariant() && idPath[cnFrom.getIndex()][cnTo.getIndex()]!=null)
					continue;
				if (ce1.getCons().isContraVariant() && idPath[cnTo.getIndex()][cnFrom.getIndex()]!=null)
					continue;
				
				if (ce1.getCons().equals(ce2.getCons())) {
					
					// check if all elements flows into another constructor
					boolean success = true;
					Edge redEdge = EmptyEdge.getInstance();

					for (int i=0; i<ce1.getCons().getArity(); i++) {
						Element e1 = ce1.getElements().get(i);
						Element e2 = ce2.getElements().get(i);
						if (idPath[g.getNode(e1).getIndex()][g.getNode(e2).getIndex()]==null
								|| e1 instanceof Variable || e2 instanceof Variable) {
							success = false;
							break;
						}
						else {
							redEdge = new LeqEdge(g.getNode(e1), g.getNode(e2), idPath[g.getNode(e1).getIndex()][g.getNode(e2).getIndex()], redEdge);
						}
					}
					if (success) {						
						Node f = cnFrom;
						Node t = cnTo;
						if (ce1.getCons().isContraVariant()) {
							t = cnFrom;
							f = cnTo;
						}
						ReductionEdge newedge = new LeqEdge(f, t, new CompEdge(f, t, env, ""), redEdge);
						newedge = new LeqEdge(f, t, newedge, new CompEdge(f, t, env, ""));
						// this number is a little ad hoc
						shortestLEQ[f.getIndex()][t.getIndex()] = newedge.getLength();
						queue.offer(newedge);
						idPath[f.getIndex()][t.getIndex()] = newedge;
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
			return idPath[sIndex][eIndex].getEdges();
		}
		else 
			return null;
	}
}
