package constraint.analysis;

import graph.CompEdge;
import graph.ConstraintGraph;
import graph.Edge;
import graph.EdgeCondition;
import graph.EmptyEdge;
import graph.JoinEdge;
import graph.LeftEdge;
import graph.LeqEdge;
import graph.MeetEdge;
import graph.Node;
import graph.Polarity;
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
import constraint.ast.Variable;

/**
 * Implements the dynamic programming algorithm proposed by Chris Barrett, Riko
 * Jacob and Madhav Marathe. More details can be found in their paper
 * "Formal-language-constrained path problems". We also handle "meet" and "join"
 * when leq edges are inferred
 */
public class ShortestPathFinder extends CFLPathFinder {
	
	/** length of shortest paths */
	private int[][] shortestLEQ;
	private Map<EdgeCondition, Integer>[][] shortestLeft;
	
	/** Lookup tables to find enumerable elements from components. These tables are used to infer extra edges for join/meet/constructors */
	private Map<Node, List<Node>>   joinElements = new HashMap<Node, List<Node>>();
	private Map<Node, List<Node>>   meetElements = new HashMap<Node, List<Node>>();
	private Map<Node, List<Node>>   consElements = new HashMap<Node, List<Node>>();
	
	/** other fields */
	private boolean CORRECTNESS_CHECK = false;
	private int MAX = 10000;
	private PriorityQueue<ReductionEdge> queue;
	
	/**
	 * @param graph
	 *            A graph to be saturated
	 */
	public ShortestPathFinder(ConstraintGraph graph) {
		super(graph);
		int size = g.getAllNodes().size();
		queue = new PriorityQueue<ReductionEdge>(
				500, new Comparator<ReductionEdge>() {
					public int compare(ReductionEdge o1, ReductionEdge o2) {
						return o1.size - o2.size;
					}
				});
		shortestLEQ = new int[size][size];
		shortestLeft = new HashMap[size][size];
		for (Node start : g.getAllNodes()) {
			for (Node end : g.getAllNodes()) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();
				shortestLeft[sIndex][eIndex] = new HashMap<EdgeCondition, Integer>();
				shortestLEQ[sIndex][eIndex] = MAX;
			}
		}
		initTables();
	}
	
	/**
	 * initialize the lookup tables
	 */
	private void initTables() {
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
			} else if (element instanceof MeetElement) {
				MeetElement je = (MeetElement) element;
				for (Element ele : je.getElements()) {
					Node toadd = g.getNode(ele);
					if (!meetElements.containsKey(toadd))
						meetElements.put(toadd, new ArrayList<Node>());
					meetElements.get(toadd).add(n);
				}
			} else if (element instanceof ConstructorApplication) {
				ConstructorApplication ce = (ConstructorApplication) element;
				for (Element ele : ce.getElements()) {
					Node toadd = g.getNode(ele);
					if (!consElements.containsKey(toadd))
						consElements.put(toadd, new ArrayList<Node>());
					consElements.get(toadd).add(n);
				}
			}
		}
	}
	
	@Override
	protected void addEdge (ReductionEdge edge) {
		int fIndex = edge.getFrom().getIndex();
		int tIndex = edge.getTo().getIndex();
		
		// LeqEdge and LeftEdge are added to the working list (queue) to infer more edges
		if (edge instanceof LeqEdge) {
			shortestLEQ[fIndex][tIndex] = 1;
			leqPath[fIndex][tIndex] = (LeqEdge) edge;
			queue.offer(edge);	
		}
		else if (edge instanceof LeftEdge) {
			LeftEdge le = (LeftEdge) edge;
			shortestLeft[fIndex][tIndex].put(le.cons, 1);
			leftPath[fIndex][tIndex].put(le.cons, le);
			queue.offer(edge);		
		}
		else if (edge instanceof RightEdge) {
			Node from = edge.getFrom();
			Node to = edge.getTo();
			
			if (!rightPath.containsKey(from)) {
				rightPath.put(from, new HashMap<Node, List<RightEdge>>());
			}
			if (!rightPath.get(from).containsKey(to)) {
				rightPath.get(from).put(to, new ArrayList<RightEdge>());
			}
			rightPath.get(from).get(to).add((RightEdge)edge);
		}
	}
	
	/**
	 * Finding the (shortest) reduction path for error diagnosis is an instance
	 * of the context-free-language-reachability problem with the following grammar:
	 * <p>
	 * leq := left right | leq leq 
	 * left := left leq
	 * <p>
	 * We follow the dynamic programming algorithm proposed by Chris Barrett, Riko
	 * Jacob and Madhav Marathe. More details can be found in their paper 
	 * "Formal-language-constrained path problems". One difference is that we also
	 * handle "meet" and "join" when id edges are inferred
	 */
	protected void saturation() {
		List<Node> allNodes = g.getAllNodes();
		
		int current_length = 0;
		while (!queue.isEmpty()) {	
			ReductionEdge edge = queue.poll();

			if (edge instanceof LeqEdge)
				tryAddingExtraEdges ((LeqEdge)edge);

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
						LeqEdge newedge = new LeqEdge(edge, leqPath[fIndex][tIndex]);
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (leqPath[sIndex][tIndex]!=null)
							queue.remove(newedge);
						queue.offer(newedge);
						leqPath[sIndex][tIndex] = newedge;
						
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

								LeftEdge newedge = new LeftEdge(ec, edge, leqPath[fIndex][tIndex]);

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

								LeftEdge newedge = new LeftEdge(ec, edge, leqPath[tIndex][fIndex].getReverse());

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
								LeqEdge newedge = new LeqEdge(edge, e);

								shortestLEQ[sIndex][tIndex] = shortestLeft[sIndex][fIndex]
										.get(ec) + 1;
								if (leqPath[sIndex][tIndex] != null)
									queue.remove(leqPath[sIndex][tIndex]);
								queue.offer(newedge);
								leqPath[sIndex][tIndex] = newedge;

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
						LeqEdge newedge = new LeqEdge(leqPath[sIndex][fIndex], edge);
						
						shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
								+ shortestLEQ[fIndex][tIndex];
						if (leqPath[sIndex][tIndex]!=null)
							queue.remove(leqPath[sIndex][tIndex]);
						queue.offer(newedge);
						leqPath[sIndex][tIndex] = newedge;

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

								LeftEdge newedge = new LeftEdge(ec, leftPath[sIndex][fIndex].get(ec), edge);

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

								LeftEdge newedge = new LeftEdge(ec, leftPath[sIndex][fIndex].get(ec), edge.getReverse());

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
						if (leqPath[startIndex][endIndex] != null
								&& shortestLEQ[startIndex][endIndex] != leqPath[startIndex][endIndex]
										.getEdges().size()) {
							System.err.println("Error: shortest id-path length is inconsistent from " + start + " to " + end);
							System.exit(1);
						}
					}
				}
			}
		}
	}
	
	/**
	 * Given a newly discovered LeqEdge, this function tries to identify extra
	 * LeqEdges by using the properties of meet, join and constructor
	 */
	private void tryAddingExtraEdges (LeqEdge edge) {
		Node from = edge.getFrom();
		Node to = edge.getTo();
		
		// if node "to" is an element of a meet label, add an leq edge from node
		// "from" to the meet element if it flows into all components
		if (meetElements.containsKey(to)) {
			for (Node meetnode : meetElements.get(to)) {
				MeetElement me = (MeetElement) meetnode.getElement();
				Node candidate = from;
				int candIndex = candidate.getIndex();
				int meetIndex = meetnode.getIndex();
				boolean success = true;
				Edge redEdge = EmptyEdge.getInstance();

				if (leqPath[candIndex][meetIndex] != null)
					continue;
				for (Element e : me.getElements()) {
					int eleIndex = g.getNode(e).getIndex();
					if (leqPath[candIndex][eleIndex] == null) {
						success = false;
						break;
					}
				}
				if (success) {
					for (Element e : me.getElements()) {
						int eleIndex = g.getNode(e).getIndex();
						redEdge = new LeqEdge(leqPath[candIndex][eleIndex], redEdge);
					}
					LeqEdge newedge = new LeqEdge(new MeetEdge(from, meetnode), redEdge);
					newedge = new LeqEdge(newedge, new MeetEdge(from, meetnode));
					shortestLEQ[candIndex][meetIndex] = newedge.getLength();
					queue.offer(newedge);
					leqPath[candIndex][meetIndex] = newedge;
				}
			}
		}
		
		// if node "from" is an element of a join label, add an leq edge from 
		// the join element to node "to" if all components flow into it
		if (joinElements.containsKey(from)) {
			for (Node joinnode : joinElements.get(from)) {
				JoinElement je = (JoinElement) joinnode.getElement();
				Node candidate = to;
				int candIndex = candidate.getIndex();
				int joinIndex = joinnode.getIndex();
				boolean success = true;
				Edge redEdge = EmptyEdge.getInstance();

				if (leqPath[joinIndex][candIndex] != null)
					continue;
				for (Element e : je.getElements()) {
					int eleIndex = g.getNode(e).getIndex();
					if (leqPath[eleIndex][candIndex] == null) {
						success = false;
						break;
					}
				}
				if (success) {
					for (Element e : je.getElements()) {
						int eleIndex = g.getNode(e).getIndex();
						redEdge = new LeqEdge(leqPath[eleIndex][candIndex], redEdge);
					}
					LeqEdge newedge = new LeqEdge(new JoinEdge(joinnode, to), redEdge);
					newedge = new LeqEdge(newedge, new JoinEdge(joinnode, to));
					// this number is a little ad hoc
					shortestLEQ[joinIndex][candIndex] = newedge.getLength();
					queue.offer(newedge);
					leqPath[joinIndex][candIndex] = newedge;
				}
			}
		}
		
		// if node "from" and "to" belong to same constructor, check if this new
		// link enables a leq relation on the constructor application
		if (consElements.containsKey(from) && consElements.containsKey(to)) {
			for (Node cnFrom : consElements.get(from)) {
				for (Node cnTo : consElements.get(to)) {
					// make sure this is "ce1", not the swapped one when the constructor is contravariant
					ConstructorApplication ce1 = (ConstructorApplication) cnFrom.getElement(); 
					ConstructorApplication ce2 = (ConstructorApplication) cnTo.getElement();

					if (!ce1.getCons().isContraVariant()
							&& leqPath[cnFrom.getIndex()][cnTo.getIndex()] != null)
						continue;
					if (ce1.getCons().isContraVariant()
							&& leqPath[cnTo.getIndex()][cnFrom.getIndex()] != null)
						continue;

					if (ce1.getCons().equals(ce2.getCons())) {
						// check if all elements flows into another constructor
						boolean success = true;
						Edge redEdge = EmptyEdge.getInstance();

						for (int i = 0; i < ce1.getCons().getArity(); i++) {
							Element e1 = ce1.getElements().get(i);
							Element e2 = ce2.getElements().get(i);
							int ie1 = g.getNode(e1).getIndex();
							int ie2 = g.getNode(e2).getIndex();
							if (leqPath[ie1][ie2] == null || e1 instanceof Variable || e2 instanceof Variable) {
								success = false;
								break;
							} else {
								redEdge = new LeqEdge(leqPath[ie1][ie2], redEdge);
							}
						}
						if (success) {
							Node f = cnFrom;
							Node t = cnTo;
							if (ce1.getCons().isContraVariant()) {
								t = cnFrom;
								f = cnTo;
							}
							LeqEdge newedge = new LeqEdge(new CompEdge(f, t, ""), redEdge);
							newedge = new LeqEdge(newedge, new CompEdge(f, t, ""));
							shortestLEQ[f.getIndex()][t.getIndex()] = newedge.getLength();
							queue.offer(newedge);
							leqPath[f.getIndex()][t.getIndex()] = newedge;
						}
					}
				}
			}
		}
	}
}
