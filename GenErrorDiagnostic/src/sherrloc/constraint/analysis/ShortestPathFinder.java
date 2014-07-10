package sherrloc.constraint.analysis;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import sherrloc.constraint.ast.Axiom;
import sherrloc.constraint.ast.Axiom.EdgeMatch;
import sherrloc.constraint.ast.ConstructorApplication;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.Inequality;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.QuantifiedVariable;
import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.Edge;
import sherrloc.graph.EdgeCondition;
import sherrloc.graph.LeftEdge;
import sherrloc.graph.LeqCondition;
import sherrloc.graph.LeqEdge;
import sherrloc.graph.LeqRevCondition;
import sherrloc.graph.Node;
import sherrloc.graph.ReductionEdge;
import sherrloc.graph.RightEdge;
import sherrloc.graph.Variance;

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
	private int MAX = 100000;
	private PriorityQueue<ReductionEdge> queue;
	private boolean DEBUG = false;
	
	/** optimazations */
	private final static boolean USE_SF = false;
	private boolean StandardForm;
	
	/**
	 * @param graph
	 *            A graph to be saturated
	 */
	public ShortestPathFinder(ConstraintGraph graph, boolean verbose, boolean isHypo) {
		super(graph);
		/** initialize data structures */
		StandardForm = USE_SF && !isHypo;
		int size = g.getAllNodes().size();
		queue = new PriorityQueue<ReductionEdge>(
				500, new Comparator<ReductionEdge>() {
					public int compare(ReductionEdge o1, ReductionEdge o2) {
						return o1.getLength() - o2.getLength();
					}
				});
		shortestLEQ = new int[size][size];
		shortestLeft = new HashMap[size][size];
		for (Node start : g.getAllNodes()) {
			for (Node end : g.getAllNodes()) {
				int sIndex = start.getIndex();
				int eIndex = end.getIndex();
				if (sIndex == eIndex)
					shortestLEQ[sIndex][eIndex] = 0;
				else
					shortestLEQ[sIndex][eIndex] = MAX;
			}
		}
		initTables();
		long startTime = System.currentTimeMillis();
		initialize();
		saturation();
		long endTime = System.currentTimeMillis();
		if (verbose)
			System.out.println("path_finding time: " + (endTime - startTime));
	}
	
	@Override
	public ConstraintGraph getGraph() {
		return g;
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
				// notice that we only need to infer extra edges for concrete
				// constructors, so there is no need to collect VariableApplication
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
	protected void inferEdge(Node start, Node end, EdgeCondition inferredType, int size, List<Triple> evidence, boolean isAtomic) {	
		if (nextHop[start.getIndex()][end.getIndex()] == null)
			nextHop[start.getIndex()][end.getIndex()] = new HashMap<EdgeCondition, List<Triple>>();
		
		nextHop[start.getIndex()][end.getIndex()].put(inferredType, evidence);
		
		if (inferredType instanceof LeqCondition) {
			if (start.equals(end))
				return;
			queue.offer(new LeqEdge(start, end, size));
			shortestLEQ[start.getIndex()][end.getIndex()] = size;
			if (isAtomic) {
				addAtomicLeqEdge(start.getIndex(), end.getIndex());
			}
		}
		else if (!inferredType.isReverse()) {
			queue.offer(new LeftEdge(start, end, size, inferredType));
			if (shortestLeft[start.getIndex()][end.getIndex()] == null)
				shortestLeft[start.getIndex()][end.getIndex()] = new HashMap<EdgeCondition, Integer>();
			shortestLeft[start.getIndex()][end.getIndex()].put(inferredType, size);
		}
		else {
			RightEdge newedge = new RightEdge(start, end, size, inferredType);
			int fIndex = start.getIndex(), tIndex = end.getIndex();
			if (!rightPath.containsKey(fIndex)) {
				rightPath.put(fIndex, new HashMap<Integer, List<RightEdge>>());
			}
			if (!rightPath.get(fIndex).containsKey(tIndex)) {
				rightPath.get(fIndex).put(tIndex, new ArrayList<RightEdge>());
			}
			rightPath.get(fIndex).get(tIndex).add(newedge);
		}
		
		if (DEBUG) {
			List<Edge> lst = new ArrayList<Edge>();
			getLeqPath(start, end, inferredType, lst, false);
			for (Edge e : lst)
				System.out.print(e.getFrom() + " --> " + e.getTo());
			System.out.println();
		}
	}	
	
	/**
	 * apply rule LEQ ::= LEQ LEQ
	 * 
	 * @param sIndex
	 *            Start node of the first LEQ edge
	 * @param fIndex
	 *            End node of the first LEQ edge (the same as the start node of
	 *            the second LEQ edge
	 * @param tIndex
	 *            End node of the second LEQ edge
	 */
	private void applyLeqLeq (Node from, Node mid, Node to) {
		if (from.equals(to))
			return;
		int sIndex = from.getIndex(), fIndex = mid.getIndex(), tIndex = to.getIndex();
		if (shortestLEQ[sIndex][fIndex] + shortestLEQ[fIndex][tIndex] < shortestLEQ[sIndex][tIndex]) {
//			if (shortestLEQ[sIndex][tIndex] < MAX)
//				System.out.println("Got a smaller edge");
			shortestLEQ[sIndex][tIndex] = shortestLEQ[sIndex][fIndex]
					+ shortestLEQ[fIndex][tIndex];
			List<Triple> evi = new ArrayList<Triple>();
			evi.add(new Triple(from, mid, LeqCondition.getInstance()));
			evi.add(new Triple(mid, to, LeqCondition.getInstance()));
			inferEdge(from, to, LeqCondition.getInstance(), shortestLEQ[sIndex][tIndex], evi, false);
		}
	}
		
	/**
	 * apply rule LEQ ::= LEFT RIGHT
	 * 
	 * @param leftS
	 *            Start node of the LEFT edge
	 * @param leftE
	 *            End node of the LEFT edge (the same as the start node of the
	 *            RIGHT edge
	 * @param rightE
	 *            End node of the RIGHT edge
	 * @param ec
	 *            Edge condition ({@link EdgeCondition}) of the LEFT edge
	 */
	private void applyLeftRight (Node from, Node mid, Node to, EdgeCondition ec) {
		int leftS = from.getIndex(), leftE = mid.getIndex(), rightE = to.getIndex();
		if (ec != null && shortestLeft[leftS][leftE]!=null &&
				hasRightEdges(leftE, rightE) && shortestLeft[leftS][leftE].get(ec) + 1 < shortestLEQ[leftS][rightE]) {
			for (RightEdge e : getRightEdges(leftE, rightE)) {
				if (e != null && ec.matches(((RightEdge) e).cons)) {
					shortestLEQ[leftS][rightE] = shortestLeft[leftS][leftE].get(ec) + 1;
					List<Triple> evi = new ArrayList<Triple>();
					evi.add(new Triple(from, mid, ec));
					evi.add(new Triple(mid, to, ((RightEdge) e).cons));
					inferEdge(from, to, LeqCondition.getInstance(), shortestLEQ[leftS][rightE], evi, true);
				}
			}
		}
	}
		
	/**
	 * apply rule LEFT ::= LEFT LEQ
	 * 
	 * @param leftS
	 *            Start node of the LEFT edge
	 * @param leftE
	 *            End node of the LEFT edge
	 * @param newE
	 *            End node of the inferred LEFT edge
	 * @param ec
	 *            Edge condition ({@link EdgeCondition}) of the LEFT edge
	 * @param useReverse
	 *            Use the reverse of LEQ edge, since the negative LEQ edges are
	 *            not explicitly represented in graph to save space
	 */
	private void applyLeftLeq (Node from, Node mid , Node to , EdgeCondition ec, boolean useReverse) {
		int leftS = from.getIndex(), leftE = mid.getIndex(), newE = to.getIndex();
		int leqS = leftE, leqE = newE;
		if (useReverse) {
			leqS = newE;
			leqE = leftE;
		}
		
		if (ec != null && shortestLeft[leftS][leftE] != null 
				&& shortestLEQ[leqS][leqE] < MAX 
				&& (shortestLeft[leftS][newE]==null || !shortestLeft[leftS][newE].containsKey(ec)
			|| shortestLeft[leftS][leftE].get(ec) + shortestLEQ[leqS][leqE] < shortestLeft[leftS][newE].get(ec))) {
				if (shortestLeft[leftS][newE] == null)
					shortestLeft[leftS][newE] = new HashMap<EdgeCondition, Integer>();
				shortestLeft[leftS][newE].put(ec, shortestLeft[leftS][leftE].get(ec) + shortestLEQ[leqS][leqE]);
				List<Triple> evi = new ArrayList<Triple>();
				evi.add(new Triple(from, mid, ec));
				if (!useReverse)
					evi.add(new Triple(mid, to, LeqCondition.getInstance()));
				else
					evi.add(new Triple(mid, to, LeqRevCondition.getInstance()));
				inferEdge(from, to, ec, shortestLeft[leftS][newE].get(ec), evi, false);
		}
	}	
		
	/**
	 * Finding the (shortest) reduction path for error diagnosis is an instance
	 * of the context-free-language-reachability problem with the following
	 * grammar:
	 * <p>
	 * leq := left right | leq leq left := left leq
	 * <p>
	 * We follow the dynamic programming algorithm proposed by Chris Barrett,
	 * Riko Jacob and Madhav Marathe. More details can be found in their paper
	 * "Formal-language-constrained path problems". We also handle contravariant
	 * parameters, "meet" and "join" when id edges are inferred
	 */
	protected void saturation() {
		Set<Node> allNodes = g.getAllNodes();
		
		int current_length = 0;
//		int count = 1;
//		long startTime = System.currentTimeMillis();
		while (!queue.isEmpty()) {	
			ReductionEdge edge = queue.poll();
//			System.out.println(count++);
//			startTime = System.currentTimeMillis();
			
			if (edge instanceof LeqEdge)
				tryAddingExtraEdges ((LeqEdge)edge);
			
			assert (current_length <= edge.getLength()) : "Error: got a smaller edge "+ current_length + " " + edge.getLength();
			
			current_length = edge.getLength();
									
			Node from = edge.getFrom();
			Node to = edge.getTo();
				
			for (Node iNode : allNodes) {
//				if (iNode.equals(from) || iNode.equals(to))
//					continue;
				
				// first, use the reduction edge as the left part of a reduction rule
				if (edge instanceof LeqEdge) { 
					// LEQ = LEQ LEQ
					if ((!StandardForm || (isDashedEdge(from) && isSolidEdge(to)))
							&& hasAtomicLeqEdge(to.getIndex(), iNode.getIndex()))
						applyLeqLeq(from, to, iNode);
				}
				else if (edge instanceof LeftEdge) {
					EdgeCondition ec = ((LeftEdge)edge).getCondition();
					
					// LEQ = LEFT RIGHT
					if (hasRightEdges(to.getIndex(), iNode.getIndex()))
						applyLeftRight(from, to, iNode, ec);

					// LEFT = LEFT LEQ (this reduction is redundant)
					if (StandardForm && hasAtomicLeqEdge(to.getIndex(), iNode.getIndex()))
						applyLeftLeq(from, to, iNode, ec, ec.getVariance()==Variance.NEG);
				}
				
				// second, use the reduction edge as the right part of a reduction rule
				if (edge instanceof LeqEdge) {
					// LEQ = LEQ LEQ
					if (StandardForm && isDashedEdge(iNode) && isSolidEdge(from)
							&& hasAtomicLeqEdge(from.getIndex(), to.getIndex()))
						applyLeqLeq(iNode, from, to);
					else if (!StandardForm && hasAtomicLeqEdge(iNode.getIndex(), from.getIndex()))
						applyLeqLeq(iNode, from, to);
	
					// LEFT := LEFT LEQ
					if (shortestLeft[iNode.getIndex()][from.getIndex()] != null) {
						// FIXME: it seems that it makes no difference to make
						// sure either LEFT or LEQ is atomic. But it turns out a test program 
						// STUDENT08/20060408-23:13:58 takes longer to run.
//						&& (!StandardForm || hasAtomicLeqEdge(from.getIndex(), to.getIndex()))) {
						for (EdgeCondition ec : shortestLeft[iNode.getIndex()][from.getIndex()].keySet()) {
							if (shortestLeft[iNode.getIndex()][from.getIndex()].get(ec)==1)
								applyLeftLeq(iNode, from, to, ec, ec.getVariance()==Variance.NEG);
						}
					}
				}
			}
		}
//		System.out.println("Saturation is done");
	}
	
	@Override
	public boolean hasLeqEdge(Node from, Node end) {
		return from.getElement().isBottom() || end.getElement().isTop() 
				|| from.getElement().equals(end.getElement()) 
				|| shortestLEQ[from.getIndex()][end.getIndex()] != MAX;
	}
	
	@Override
	public boolean hasLeftEdge(Node from, Node end) {
		if (shortestLeft[from.getIndex()][end.getIndex()] == null ||
				shortestLeft[from.getIndex()][end.getIndex()].isEmpty())
			return false;
		else
			return true;
	}
	
	/**
	 * Return a path in the constraint graph so that a LEFT edge on
	 * <code>start, end</code> can be derived from constraints along the path.
	 * Return null when no such path exits
	 */
	public List<List<Edge>> getLeftPaths(Node start, Node end) {
		List<List<Edge>> paths = new ArrayList<List<Edge>>();
		if (!hasLeftEdge(start, end))
			return new ArrayList<List<Edge>>();
		else {
			for (EdgeCondition con : shortestLeft[start.getIndex()][end.getIndex()].keySet()) {
				List<Edge> lst = new ArrayList<Edge>();
				getLeqPath(start, end, con, lst, false);
				paths.add(lst);
			}
		}
		return paths;
	}
	
	/**
	 * Try to apply axioms that might utilized the newly added LeqEdge edge to
	 * infer new edges in the graph
	 */
	private void applyAxioms (LeqEdge edge) {
		for (Axiom rule : g.getRules()) {
			if (!rule.mayMatch(edge))
				continue;
			List<Map<QuantifiedVariable, Element>> maps = rule.findMatchesInPremise(this);
			// apply all substitutions along the unification to conclusion
			for (Inequality ieq : rule.getConclusion()) {
				List<EdgeMatch> emlst = rule.findMatches (ieq.getFirstElement(), ieq.getSecondElement(), this, maps);
				for (EdgeMatch em : emlst) {
					if (!hasLeqEdge(em.n1, em.n2)) {
						inferEdge(em.n1, em.n2, LeqCondition.getInstance(), 1,
								new ArrayList<Triple>(), true);
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
		
		applyAxioms(edge);
		
		// if node "to" is an element of a meet label, add an leq edge from node
		// "from" to the meet element if it flows into all components
		if (meetElements.containsKey(to)) {
			for (Node meetnode : meetElements.get(to)) {
				MeetElement me = (MeetElement) meetnode.getElement();
				Node candidate = from;
				int candIndex = candidate.getIndex();
				int meetIndex = meetnode.getIndex();
				boolean success = true;
				
				if (hasLeqEdge(candidate, meetnode) || candIndex == meetIndex)
					continue;
				for (Element e : me.getElements()) {
					if (!hasLeqEdge(candidate, g.getNode(e))) {
						success = false;
						break;
					}
				}
				if (success) {
					List<Triple> evidences = new ArrayList<Triple>();
					int size = 0;
					for (Element e : me.getElements()) {
						int eleIndex = g.getNode(e).getIndex();
						if (shortestLEQ[candIndex][eleIndex] < MAX) {
							size += shortestLEQ[candIndex][eleIndex];
							evidences.add(new Triple(candidate, g.getNode(e), LeqCondition.getInstance()));
						}
						else {
							size ++;
						}
					}
					inferEdge(candidate, meetnode, LeqCondition.getInstance(), size, evidences, true);
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

				if (hasLeqEdge(joinnode, candidate) || joinIndex == candIndex)
					continue;
				for (Element e : je.getElements()) {
					if (!hasLeqEdge(g.getNode(e), candidate)) {
						success = false;
						break;
					}
				}
				if (success) {
					List<Triple> evidences = new ArrayList<Triple>();
					int size = 0;
					for (Element e : je.getElements()) {
						int eleIndex = g.getNode(e).getIndex();
						if (shortestLEQ[eleIndex][candIndex] < MAX) {
							size += shortestLEQ[eleIndex][candIndex];
							evidences.add(new Triple(g.getNode(e), candidate, LeqCondition.getInstance()));
						}
						else {
							size++;
						}
					}
					inferEdge(joinnode, candidate, LeqCondition.getInstance(), size, evidences, true);
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
					
					if (!ce1.getCons().equals(ce2.getCons()))
						continue;
					
					// depending on the variance of the parameters, we need to
					// infer an edge from cnFrom to cnTo (covariant), cnTo to cnFrom
					// (contravariant), or both (invariant)
					boolean ltor = false, rtol = false;
					if (ce1.getCons().getVariance().equals(Variance.POS))
						ltor = true;
					else if (ce1.getCons().getVariance().equals(Variance.NEG))
						rtol = true;
					else if (ce1.getCons().getVariance().equals(Variance.NONE)) {
						ltor = true;
						rtol = true;
					} 

					// only infer new edges when none already exists
					if ( (ltor && !hasLeqEdge(cnFrom, cnTo)) || (rtol && !hasLeqEdge(cnTo, cnFrom)) ) {
						// check if all elements flows into another constructor
						boolean success = true;

						for (int i = 0; i < ce1.getCons().getArity(); i++) {
							Element e1 = ce1.getElements().get(i);
							Element e2 = ce2.getElements().get(i);
							if (!hasLeqEdge(g.getNode(e1), g.getNode(e2))) {
								success = false;
								break;
							}
							// test the other direction for invariant parameters
							if (ce1.getCons().getVariance().equals(Variance.NONE)) {
								if (!hasLeqEdge(g.getNode(e2), g.getNode(e1))) {
									success = false;
									break;
								}
							}
						}
												
						if (success) {
							List<Triple> evidences = new ArrayList<Triple>();
							int size=0;
							for (int i = 0; i < ce1.getCons().getArity(); i++) {
								Element e1 = ce1.getElements().get(i);
								Element e2 = ce2.getElements().get(i);
								if (shortestLEQ[g.getNode(e1).getIndex()][g.getNode(e2).getIndex()] < MAX) {
									size += shortestLEQ[g.getNode(e1).getIndex()][g.getNode(e2).getIndex()];
									evidences.add(new Triple(g.getNode(e1), g.getNode(e2), LeqCondition.getInstance()));
								}
								else {
									size ++;
								}
							}
							if (ltor && !hasLeqEdge(cnFrom, cnTo))
								inferEdge(cnFrom, cnTo, LeqCondition.getInstance(), size, evidences, true);
							if (rtol && !hasLeqEdge(cnTo, cnFrom))
								inferEdge(cnTo, cnFrom, LeqCondition.getInstance(), size, evidences, true);
						}
					}
				}
			}
		}
	}
}