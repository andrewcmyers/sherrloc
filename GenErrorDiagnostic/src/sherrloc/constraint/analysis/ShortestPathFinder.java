package sherrloc.constraint.analysis;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import sherrloc.constraint.ast.Application;
import sherrloc.constraint.ast.Axiom;
import sherrloc.constraint.ast.Axiom.EdgeMatch;
import sherrloc.constraint.ast.Axiom.PremiseMatch;
import sherrloc.constraint.ast.ConstructorApplication;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.FunctionApplication;
import sherrloc.constraint.ast.Inequality;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.Relation;
import sherrloc.constraint.ast.Variable;
import sherrloc.constraint.ast.Constructor;
import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.Edge;
import sherrloc.graph.EdgeCondition;
import sherrloc.graph.HypothesisSet;
import sherrloc.graph.LeftEdge;
import sherrloc.graph.LeqCondition;
import sherrloc.graph.LeqEdge;
import sherrloc.graph.LeqRevCondition;
import sherrloc.graph.Node;
import sherrloc.graph.ReductionEdge;
import sherrloc.graph.RightEdge;
import sherrloc.graph.Variance;

/**
 * Implements the dynamic programming algorithm proposed by Chris Barrett, Riko Jacob and Madhav
 * Marathe. More details can be found in their paper "Formal-language-constrained path problems". We
 * also handle "meet" and "join" when leq edges are inferred
 */
public class ShortestPathFinder extends CFLPathFinder {

    /**
     * length of shortest paths
     */
//	private int[][] shortestLEQ;
//	private Map<EdgeCondition, Integer>[][] shortestLeft;
    // For each pair of nodes (and edge condition), the length of the shortest derivation for each
    // minimal set of hypotheses: no recorded set contains another.
    private Map<Integer, Map<Integer, Map<HypothesisSet, Integer>>> shortestLEQ;
    private Map<Integer, Map<Integer, Map<EdgeCondition, Map<HypothesisSet, Integer>>>> shortestLeft;

    /**
     * Lookup tables to find enumerable elements from components. These tables are used to infer
     * extra edges for join/meet/constructors
     */
    private Map<Node, List<Node>> joinElements = new HashMap<Node, List<Node>>();
    private Map<Node, List<Node>> meetElements = new HashMap<Node, List<Node>>();
    private Map<Node, List<Node>> consElements = new HashMap<Node, List<Node>>();

    /**
     * other fields
     */
    private int MAX = 100000;
    private PriorityQueue<ReductionEdge> queue;
    private boolean DEBUG = false;
    private boolean ACTIVE = true;

    /**
     * optimizations
     */
    private final static boolean USE_SF = false;
    private boolean standardForm;
    private boolean actively_expanding;

    /**
     * @param graph A graph to be saturated
     */
    public ShortestPathFinder(ConstraintGraph graph, boolean verbose, boolean isHypo) {
        super(graph);
        /** initialize data structures */
        standardForm = USE_SF && !isHypo;
        actively_expanding = ACTIVE && isHypo;
        queue = new PriorityQueue<ReductionEdge>(
                500, new Comparator<ReductionEdge>() {
            public int compare(ReductionEdge o1, ReductionEdge o2) {
                return o1.getLength() - o2.getLength();
            }
        });
        shortestLEQ = new HashMap<>();
        shortestLeft = new HashMap<>();
//		for (int i=0; i<size; i++) {
//			for (int j=0; j<size; j++) {
//				if (i == j)
//					shortestLEQ[i][j] = 0;
//				else
//					shortestLEQ[i][j] = MAX;
//			}
//		}
        initTables();
        long startTime = System.currentTimeMillis();
        initialize();
        saturation();
        long endTime = System.currentTimeMillis();
        if (verbose) {
            System.out.println("path_finding time: " + (endTime - startTime));
        }
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
                    if (!joinElements.containsKey(toadd)) {
                        joinElements.put(toadd, new ArrayList<Node>());
                    }
                    joinElements.get(toadd).add(n);
                }
            } else if (element instanceof MeetElement) {
                MeetElement je = (MeetElement) element;
                for (Element ele : je.getElements()) {
                    Node toadd = g.getNode(ele);
                    if (!meetElements.containsKey(toadd)) {
                        meetElements.put(toadd, new ArrayList<Node>());
                    }
                    meetElements.get(toadd).add(n);
                }
            } else if (element instanceof ConstructorApplication
                    || element instanceof FunctionApplication) {
                // notice that we only need to infer extra edges for concrete
                // constructors and functions, so there is no need to collect VariableApplication
                // Need to take care of nested applications (see test "consofvar4").
                initConsElements((Application) element, n);
            }
        }
    }

    /**
     * Add n as the super-element of all its sub-elements.
     *
     * @param app An application element
     * @param n   The node for parameter app
     */
    private void initConsElements(Application app, Node n) {
        for (Element ele : app.getElements()) {
            if (ele instanceof Application) {
                initConsElements((Application) ele, n);
            } else {
                Node toadd = g.getNode(ele);
                if (!consElements.containsKey(toadd)) {
                    consElements.put(toadd, new ArrayList<Node>());
                }
                consElements.get(toadd).add(n);
            }
        }
    }

    /** The derivations of LEQ edges from {@code start} to {@code end}; do not modify. */
    private Map<HypothesisSet, Integer> leqTable(Node start, Node end) {
        Map<Integer, Map<HypothesisSet, Integer>> m = shortestLEQ.get(start.getIndex());
        Map<HypothesisSet, Integer> t = m == null ? null : m.get(end.getIndex());
        return t == null ? Map.of() : t;
    }

    /** The derivations of LEFT edges with condition {@code ec}; do not modify. */
    private Map<HypothesisSet, Integer> leftTable(Node start, Node end, EdgeCondition ec) {
        Map<Integer, Map<EdgeCondition, Map<HypothesisSet, Integer>>> m =
                shortestLeft.get(start.getIndex());
        Map<EdgeCondition, Map<HypothesisSet, Integer>> byCond = m == null ? null
                : m.get(end.getIndex());
        Map<HypothesisSet, Integer> t = byCond == null ? null : byCond.get(ec);
        return t == null ? Map.of() : t;
    }

    private static int shortest(Map<HypothesisSet, Integer> table) {
        int min = Integer.MAX_VALUE;
        for (int d : table.values()) {
            min = Math.min(min, d);
        }
        return min;
    }

    private int getShortestLeq(Node start, Node end) {
        Map<HypothesisSet, Integer> t = leqTable(start, end);
        return t.isEmpty() ? MAX : shortest(t);
    }

    // assume hasLeft(start, end)
    private int getShortestLeft(Node start, Node end, EdgeCondition inferredType) {
        return shortest(leftTable(start, end, inferredType));
    }

    private boolean hasShortestLeft(Node start, Node end, EdgeCondition inferredType) {
        return !leftTable(start, end, inferredType).isEmpty();
    }

    /**
     * Whether a derivation with hypotheses {@code hyps} and length {@code size} should be recorded
     * in {@code table}: no recorded derivation has fewer hypotheses, or the same hypotheses and a
     * length no greater.
     */
    private static boolean improves(Map<HypothesisSet, Integer> table, HypothesisSet hyps,
            int size) {
        for (Map.Entry<HypothesisSet, Integer> e : table.entrySet()) {
            if (e.getKey().equals(hyps)) {
                if (e.getValue() <= size) {
                    return false;
                }
            } else if (e.getKey().isSubsetOf(hyps)) {
                return false;
            }
        }
        return true;
    }

    /** Records a derivation in {@code table}, dropping derivations with more hypotheses. */
    private static void record(Map<HypothesisSet, Integer> table, HypothesisSet hyps, int size) {
        table.keySet().removeIf(h -> !h.equals(hyps) && hyps.isSubsetOf(h));
        table.put(hyps, size);
    }

    @Override
    protected HypothesisSet shortestHypotheses(Node start, Node end, EdgeCondition type) {
        Map<HypothesisSet, Integer> table = type instanceof LeqCondition ? leqTable(start, end)
                : leftTable(start, end, type);
        HypothesisSet best = null;
        int bestSize = Integer.MAX_VALUE;
        for (Map.Entry<HypothesisSet, Integer> e : table.entrySet()) {
            if (e.getValue() < bestSize || (e.getValue() == bestSize
                    && e.getKey().size() < best.size())) {
                best = e.getKey();
                bestSize = e.getValue();
            }
        }
        if (best == null) {
            // a RIGHT edge, or an original edge only: use its recorded evidence
            Map<Integer, Map<EdgeCondition, Map<HypothesisSet, List<Evidence>>>> fromStart =
                    nextHop.get(start.getIndex());
            if (fromStart != null && fromStart.containsKey(end.getIndex())
                    && fromStart.get(end.getIndex()).containsKey(type)) {
                return fromStart.get(end.getIndex()).get(type).keySet().iterator().next();
            }
            return HypothesisSet.EMPTY;
        }
        return best;
    }

    @Override
    protected List<HypothesisSet> leqHypotheses(Node start, Node end) {
        List<HypothesisSet> ret = new ArrayList<>(leqTable(start, end).keySet());
        // a deterministic order: fewest hypotheses first
        ret.sort(Comparator.comparingInt(HypothesisSet::size)
                .thenComparing(HypothesisSet::toString));
        return ret;
    }

    @Override
    protected void inferEdge(Node start, Node end, EdgeCondition inferredType, int size,
            List<Evidence> evidence, boolean isAtomic, HypothesisSet hyps) {
        if (inferredType instanceof LeqCondition) {
            if (start.equals(end)) {
                addNextHop(start, end, inferredType, hyps, evidence);
                return;
            }
            if (isAtomic) {
                addAtomicLeqEdge(start.getIndex(), end.getIndex());
            }
            Map<HypothesisSet, Integer> table = shortestLEQ
                    .computeIfAbsent(start.getIndex(), k -> new HashMap<>())
                    .computeIfAbsent(end.getIndex(), k -> new HashMap<>());
            if (!improves(table, hyps, size)) {
                return;
            }
            record(table, hyps, size);
            addNextHop(start, end, inferredType, hyps, evidence);
            queue.offer(new LeqEdge(start, end, size, hyps));
        } else if (!inferredType.isReverse()) {
            Map<HypothesisSet, Integer> table = shortestLeft
                    .computeIfAbsent(start.getIndex(), k -> new HashMap<>())
                    .computeIfAbsent(end.getIndex(), k -> new HashMap<>())
                    .computeIfAbsent(inferredType, k -> new HashMap<>());
            if (!improves(table, hyps, size)) {
                return;
            }
            record(table, hyps, size);
            addNextHop(start, end, inferredType, hyps, evidence);
            queue.offer(new LeftEdge(start, end, size, inferredType, hyps));
        } else {
            addNextHop(start, end, inferredType, hyps, evidence);
            RightEdge newedge = new RightEdge(start, end, size, inferredType, hyps);
            int fIndex = start.getIndex(), tIndex = end.getIndex();
            List<RightEdge> edges = rightPath.computeIfAbsent(fIndex, k -> new HashMap<>())
                    .computeIfAbsent(tIndex, k -> new ArrayList<>());
            if (!edges.contains(newedge) || edges.stream()
                    .noneMatch(e -> e.equals(newedge) && e.getHypotheses().equals(hyps))) {
                edges.add(newedge);
            }
        }

        if (DEBUG) {
            List<Edge> lst = new ArrayList<Edge>();
            getLeqPath(start, end, inferredType, hyps, lst, false);
            for (Edge e : lst) {
                System.out.print(e.getFrom() + " --> " + e.getTo());
            }
            System.out.println();
        }
    }

    /**
     * apply rule LEQ ::= LEQ LEQ
     *
     * @param sIndex Start node of the first LEQ edge
     * @param fIndex End node of the first LEQ edge (the same as the start node of the second LEQ
     *               edge
     * @param tIndex End node of the second LEQ edge
     */
    private void applyLeqLeq(Node from, Node mid, Node to) {
        if (from.equals(to)) {
            return;
        }
        for (Map.Entry<HypothesisSet, Integer> sf : List.copyOf(leqTable(from, mid).entrySet())) {
            for (Map.Entry<HypothesisSet, Integer> ft : List.copyOf(leqTable(mid, to).entrySet())) {
                List<Evidence> evi = new ArrayList<Evidence>();
                evi.add(new Evidence(from, mid, LeqCondition.getInstance(), sf.getKey()));
                evi.add(new Evidence(mid, to, LeqCondition.getInstance(), ft.getKey()));
                inferEdge(from, to, LeqCondition.getInstance(), sf.getValue() + ft.getValue(), evi,
                        false, sf.getKey().union(ft.getKey()));
            }
        }
    }

    /**
     * apply rule LEQ ::= LEFT RIGHT
     *
     * @param from Start node of the LEFT edge
     * @param mid  End node of the LEFT edge (the same as the start node of the RIGHT edge
     * @param to   End node of the RIGHT edge
     * @param ec   Edge condition ({@link EdgeCondition}) of the LEFT edge
     */
    private void applyLeftRight(Node from, Node mid, Node to, EdgeCondition ec) {
        if (ec == null || !hasShortestLeft(from, mid, ec) || !hasRightEdges(mid, to)) {
            return;
        }
        for (Map.Entry<HypothesisSet, Integer> left
                : List.copyOf(leftTable(from, mid, ec).entrySet())) {
            for (RightEdge e : List.copyOf(getRightEdges(mid, to))) {
                if (e != null && ec.matches(e.cons)) {
                    List<Evidence> evi = new ArrayList<Evidence>();
                    evi.add(new Evidence(from, mid, ec, left.getKey()));
                    evi.add(new Evidence(mid, to, e.cons, e.getHypotheses()));
                    inferEdge(from, to, LeqCondition.getInstance(), left.getValue() + 1, evi, true,
                            left.getKey().union(e.getHypotheses()));
                }
            }
        }
    }

    private void applyRightLeft(Node from, Node mid, Node to, EdgeCondition ec) {
        if (ec == null || !hasShortestLeft(mid, to, ec) || !hasRightEdges(from, mid)) {
            return;
        }
        for (Map.Entry<HypothesisSet, Integer> left
                : List.copyOf(leftTable(mid, to, ec).entrySet())) {
            for (RightEdge e : List.copyOf(getRightEdges(from, mid))) {
                if (e != null && ec.matches(e.cons)) {
                    List<Evidence> evi = new ArrayList<Evidence>();
                    evi.add(new Evidence(from, mid, e.cons, e.getHypotheses()));
                    evi.add(new Evidence(mid, to, ec, left.getKey()));
                    inferEdge(from, to, LeqCondition.getInstance(), left.getValue() + 1, evi, true,
                            left.getKey().union(e.getHypotheses()));
                }
            }
        }
    }

    /**
     * apply rule LEFT ::= LEFT LEQ
     *
     * @param from          Start node of the LEFT edge
     * @param mid           End node of the LEFT edge
     * @param to            End node of the inferred LEFT edge
     * @param ec            Edge condition ({@link EdgeCondition}) of the LEFT edge
     * @param useReverse    Use the reverse of LEQ edge, since the negative LEQ edges are not
     *                      explicitly represented in graph to save space
     * @param onlyAtomicLeft Use only LEFT edges of length 1
     */
    private void applyLeftLeq(Node from, Node mid, Node to, EdgeCondition ec, boolean useReverse,
            boolean onlyAtomicLeft) {
        Node leqS = mid, leqE = to;
        if (useReverse) {
            leqS = to;
            leqE = mid;
        }
        if (ec == null) {
            return;
        }
        for (Map.Entry<HypothesisSet, Integer> left
                : List.copyOf(leftTable(from, mid, ec).entrySet())) {
            if (onlyAtomicLeft && left.getValue() != 1) {
                continue;
            }
            for (Map.Entry<HypothesisSet, Integer> leq
                    : List.copyOf(leqTable(leqS, leqE).entrySet())) {
                List<Evidence> evi = new ArrayList<Evidence>();
                evi.add(new Evidence(from, mid, ec, left.getKey()));
                if (!useReverse) {
                    evi.add(new Evidence(mid, to, LeqCondition.getInstance(), leq.getKey()));
                } else {
                    evi.add(new Evidence(mid, to, LeqRevCondition.getInstance(), leq.getKey()));
                }
                inferEdge(from, to, ec, left.getValue() + leq.getValue(), evi, false,
                        left.getKey().union(leq.getKey()));
            }
        }
    }

    /**
     * Finding the (shortest) reduction path for error diagnosis is an instance of the
     * context-free-language-reachability problem with the following grammar:
     * <p>
     * leq := left right | leq leq left := left leq
     * <p>
     * We follow the dynamic programming algorithm proposed by Chris Barrett, Riko Jacob and Madhav
     * Marathe. More details can be found in their paper "Formal-language-constrained path
     * problems". We also handle contravariant parameters, "meet" and "join" when id edges are
     * inferred
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

            if (edge instanceof LeqEdge) {
                tryAddingExtraEdges((LeqEdge) edge);
            }

            assert (current_length <= edge.getLength()) :
                    "Error: got a smaller edge " + current_length + " " + edge.getLength();

            current_length = edge.getLength();

            Node from = edge.getFrom();
            Node to = edge.getTo();

            for (Node iNode : allNodes) {
//				if (iNode.equals(from) || iNode.equals(to))
//					continue;

                // first, use the reduction edge as the left part of a reduction rule
                if (edge instanceof LeqEdge) {
                    // LEQ = LEQ LEQ
                    if ((!standardForm || (isDashedEdge(from) && isSolidEdge(to)))
                            && hasAtomicLeqEdge(to.getIndex(), iNode.getIndex())) {
                        applyLeqLeq(from, to, iNode);
                    }
                } else if (edge instanceof LeftEdge) {
                    EdgeCondition ec = ((LeftEdge) edge).getCondition();

                    // LEQ = LEFT RIGHT
                    if (hasRightEdges(to, iNode)) {
                        applyLeftRight(from, to, iNode, ec);
                    }

                    // LEFT = LEFT LEQ (this reduction is redundant)
                    if (standardForm && hasAtomicLeqEdge(to.getIndex(), iNode.getIndex())) {
                        applyLeftLeq(from, to, iNode, ec, ec.getVariance() == Variance.NEG, false);
                    }
                }

                // second, use the reduction edge as the right part of a reduction rule
                if (edge instanceof LeqEdge) {
                    // LEQ = LEQ LEQ
                    if (standardForm && isDashedEdge(iNode) && isSolidEdge(from)
                            && hasAtomicLeqEdge(from.getIndex(), to.getIndex())) {
                        applyLeqLeq(iNode, from, to);
                    } else if (!standardForm && hasAtomicLeqEdge(iNode.getIndex(),
                            from.getIndex())) {
                        applyLeqLeq(iNode, from, to);
                    }

                    // LEFT := LEFT LEQ
                    if (hasLeftEdge(iNode, from)) {
                        // FIXME: it seems that it makes no difference to make
                        // sure either LEFT or LEQ is atomic. But it turns out a test program
                        // STUDENT08/20060408-23:13:58 takes longer to run.
//						&& (!StandardForm || hasAtomicLeqEdge(from.getIndex(), to.getIndex()))) {
                        for (EdgeCondition ec : List.copyOf(shortestLeft.get(iNode.getIndex())
                                .get(from.getIndex()).keySet())) {
                            applyLeftLeq(iNode, from, to, ec, ec.getVariance() == Variance.NEG,
                                    true);
                        }
                    }
                }

                if (edge instanceof LeftEdge) {
                    EdgeCondition ec = ((LeftEdge) edge).getCondition();
                    Element cons = ec.getCon();
                    // LEQ = RIGHT LEFT
                    if (hasRightEdges(iNode, from) && cons instanceof Constructor
                            && ((Constructor) cons).getArity() == 1) {
                        applyRightLeft(iNode, from, to, ec);
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
                || getShortestLeq(from, end) != MAX;
    }

    @Override
    public Set<Node> getFlowsFrom(Node to) {
        Set<Node> ret = new HashSet<Node>();
        for (Integer idx : shortestLEQ.keySet()) {
            if (shortestLEQ.get(idx).containsKey(to.getIndex())) {
                ret.add(getGraph().getNode(idx));
            }
        }
        return ret;
    }

    @Override
    public Set<Node> getFlowsTo(Node from) {
        Set<Node> ret = new HashSet<Node>();
        if (shortestLEQ.containsKey(from.getIndex())) {
            for (Integer idx : shortestLEQ.get(from.getIndex()).keySet()) {
                ret.add(getGraph().getNode(idx));
            }
        }
        return ret;
    }

    @Override
    public int leqEdgeLength(Node from, Node end) {
        if (from.getElement().isBottom() || end.getElement().isTop()) {
            return 1;
        } else if (from.getElement().equals(end.getElement())) {
            return 0;
        } else {
            return getShortestLeq(from, end);
        }
    }

    @Override
    public boolean hasLeftEdge(Node from, Node to) {
        if (shortestLeft.containsKey(from.getIndex()) && shortestLeft.get(from.getIndex())
                .containsKey(to.getIndex())
                && !shortestLeft.get(from.getIndex()).get(to.getIndex()).isEmpty()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Return a path in the constraint graph so that a LEFT edge on
     * <code>start, end</code> can be derived from constraints along the path.
     * Return null when no such path exits
     */
    public List<List<Edge>> getLeftPaths(Node start, Node end) {
        List<List<Edge>> paths = new ArrayList<List<Edge>>();
        if (!hasLeftEdge(start, end)) {
            return new ArrayList<List<Edge>>();
        } else {
            for (EdgeCondition con : shortestLeft.get(start.getIndex()).get(end.getIndex())
                    .keySet()) {
                for (HypothesisSet hyps : leftTable(start, end, con).keySet()) {
                    List<Edge> lst = new ArrayList<Edge>();
                    getLeqPath(start, end, con, hyps, lst, false);
                    paths.add(lst);
                }
            }
        }
        return paths;
    }

    /**
     * Try to apply axioms that might utilized the newly added LeqEdge edge to infer new edges in
     * the graph
     */
    private void applyAxioms(LeqEdge edge) {
        for (Axiom rule : g.getRules()) {
            if (!rule.mayMatch(edge)) {
                continue;
            }
            List<PremiseMatch> pmatches = rule.findMatchesInPremise(this);

            for (PremiseMatch pmatch : pmatches) {
                // apply all substitutions along the unification to conclusion
                for (Inequality ieq : rule.getConclusion()) {
                    Element e1 = ieq.getFirstElement().subst(pmatch.map);
                    Element e2 = ieq.getSecondElement().subst(pmatch.map);

                    // expand new graph nodes when necessary
                    boolean needExpansion = actively_expanding && !e1.hasQVars() && !e2.hasQVars()
                            && pmatch.noGrayNodes && (g.hasElement(e1) || g.hasElement(e2));
                    if (needExpansion) {
                        if (!g.hasElement(e1)) {
                            g.getNode(e1, true);
                        }
                        if (!g.hasElement(e2)) {
                            g.getNode(e2, true);
                        }
                    }
                    List<EdgeMatch> emlst = rule.findMatches(ieq.getFirstElement(),
                            ieq.getSecondElement(), this, pmatch.map);
                    List<Evidence> evidences = resolve(pmatch.evidences);
                    HypothesisSet hyps = hypothesesOf(evidences);
                    for (EdgeMatch em : emlst) {
                        if (pmatch.size < getShortestLeq(em.n1, em.n2)) {
                            inferEdge(em.n1, em.n2, LeqCondition.getInstance(), pmatch.size,
                                    evidences, true, hyps);
                        }
                        if (ieq.getRelation() == Relation.EQ && pmatch.size < getShortestLeq(em.n2,
                                em.n1)) {
                            inferEdge(em.n2, em.n1, LeqCondition.getInstance(), pmatch.size,
                                    evidences, true, hyps);
                        }
                    }
                }
            }
        }
    }

    /** {@code evidences}, with each one that names no hypotheses taken as the shortest. */
    private List<Evidence> resolve(List<Evidence> evidences) {
        List<Evidence> ret = new ArrayList<>();
        for (Evidence e : evidences) {
            ret.add(e.hypotheses != null ? e
                    : new Evidence(e.start, e.end, e.ty, shortestHypotheses(e.start, e.end, e.ty)));
        }
        return ret;
    }

    /** The union of the hypotheses of resolved {@code evidences}. */
    private static HypothesisSet hypothesesOf(List<Evidence> evidences) {
        HypothesisSet hyps = HypothesisSet.EMPTY;
        for (Evidence e : evidences) {
            hyps = hyps.union(e.hypotheses);
        }
        return hyps;
    }

    /** Whether {@code from <= to} holds without any derivation. */
    private static boolean trivialLeq(Node from, Node to) {
        return from.getElement().isBottom() || to.getElement().isTop()
                || from.getElement().equals(to.getElement());
    }

    /** One way of deriving a component of a compound LEQ edge. */
    private record Option(HypothesisSet hyps, int size, Evidence evidence) {
    }

    /**
     * The ways of deriving {@code from <= to}: one per minimal set of hypotheses, or a single
     * evidence-free option if it holds without derivation. Empty if it cannot be derived.
     */
    private List<Option> leqOptions(Node from, Node to) {
        List<Option> ret = new ArrayList<>();
        for (Map.Entry<HypothesisSet, Integer> e : leqTable(from, to).entrySet()) {
            ret.add(new Option(e.getKey(), e.getValue(),
                    new Evidence(from, to, LeqCondition.getInstance(), e.getKey())));
        }
        if (ret.isEmpty() && trivialLeq(from, to)) {
            ret.add(new Option(HypothesisSet.EMPTY, 1, null));
        }
        return ret;
    }

    /**
     * Infers an LEQ edge from {@code from} to {@code to} for each combination of one way of
     * deriving each of {@code components}, which are all required.
     */
    private void inferFromComponents(Node from, Node to, List<List<Option>> components) {
        List<Option> combos = new ArrayList<>();
        combos.add(new Option(HypothesisSet.EMPTY, 0, null));
        List<List<Evidence>> evidences = new ArrayList<>();
        evidences.add(new ArrayList<>());
        for (List<Option> options : components) {
            List<Option> nextCombos = new ArrayList<>();
            List<List<Evidence>> nextEvidences = new ArrayList<>();
            for (int i = 0; i < combos.size(); i++) {
                for (Option o : options) {
                    nextCombos.add(new Option(combos.get(i).hyps().union(o.hyps()),
                            combos.get(i).size() + o.size(), null));
                    List<Evidence> evi = new ArrayList<>(evidences.get(i));
                    if (o.evidence() != null) {
                        evi.add(o.evidence());
                    }
                    nextEvidences.add(evi);
                }
            }
            combos = nextCombos;
            evidences = nextEvidences;
        }
        for (int i = 0; i < combos.size(); i++) {
            inferEdge(from, to, LeqCondition.getInstance(), combos.get(i).size(), evidences.get(i),
                    true, combos.get(i).hyps());
        }
    }

    /**
     * Actively create a new gray node (CONS subst.elem) if (CONS n.elem) is already in graph, as
     * well as n and subst are not gray nodes. The direction between (CONS subst.elem) and (CONS
     * n.elem) is determined by ltor and the variance of CONS: (CONS n.elem) --> (CONS subst.elem)
     * iff ltor&&COVAR or !ltor&&CONTRA
     *
     * @param n
     * @param subst
     */
    private void expandOneNode(Node n, Node subst, boolean ltor) {
        List<Option> options = leqOptions(n, subst);

        if (consElements.containsKey(n) && !n.isGray() && !subst.isGray()) {
            for (Node cplxNode : consElements.get(n)) {
                if (!cplxNode.isGray()) {
                    Application app = (Application) cplxNode.getElement();
                    List<Application> nelems = app.replace(n.getElement(), subst.getElement());
                    boolean direction = (ltor && (app.getVariance() == Variance.POS)) || (!ltor && (
                            app.getVariance() == Variance.NEG));
                    for (Application nelem : nelems) {
                        Node newnode = g.getNode(nelem, true);
                        if (direction && !hasLeftEdge(cplxNode, newnode)) {
                            inferFromComponents(cplxNode, newnode, List.of(options));
                        } else {
                            inferFromComponents(newnode, cplxNode, List.of(options));
                        }
                    }
                }
            }
        }
    }

    /**
     * Given a newly discovered LeqEdge, this function tries to identify extra LeqEdges by using the
     * properties of meet, join and constructor
     */
    private void tryAddingExtraEdges(LeqEdge edge) {
        Node from = edge.getFrom();
        Node to = edge.getTo();
        applyAxioms(edge);

        // if node "to" is an element of a meet label, add an leq edge from node
        // "from" to the meet element if it flows into all components
        if (meetElements.containsKey(to)) {
            for (Node meetnode : meetElements.get(to)) {
                MeetElement me = (MeetElement) meetnode.getElement();
                Node candidate = from;
                if (trivialLeq(candidate, meetnode)
                        || candidate.getIndex() == meetnode.getIndex()) {
                    continue;
                }
                List<List<Option>> components = new ArrayList<>();
                for (Element e : me.getElements()) {
                    components.add(leqOptions(candidate, g.getNode(e)));
                }
                if (components.stream().noneMatch(List::isEmpty)) {
                    inferFromComponents(candidate, meetnode, components);
                }
            }
        }

        // if node "from" is an element of a join label, add an leq edge from
        // the join element to node "to" if all components flow into it
        if (joinElements.containsKey(from)) {
            for (Node joinnode : joinElements.get(from)) {
                JoinElement je = (JoinElement) joinnode.getElement();
                Node candidate = to;
                if (trivialLeq(joinnode, candidate)
                        || joinnode.getIndex() == candidate.getIndex()) {
                    continue;
                }
                List<List<Option>> components = new ArrayList<>();
                for (Element e : je.getElements()) {
                    components.add(leqOptions(g.getNode(e), candidate));
                }
                if (components.stream().noneMatch(List::isEmpty)) {
                    inferFromComponents(joinnode, candidate, components);
                }
            }
        }

        // if node "from" and "to" belong to same constructor, check if this new
        // link enables a leq relation on the constructor application
        if (actively_expanding && (consElements.containsKey(from) || consElements.containsKey(
                to))) {
            expandOneNode(from, to, true);
            expandOneNode(to, from, false);

            if (consElements.containsKey(from) && consElements.containsKey(to)) {
                for (Node cnFrom : consElements.get(from)) {
                    for (Node cnTo : consElements.get(to)) {
                        // make sure this is "ce1", not the swapped one when the constructor is contravariant
                        // the elements can either be ConstructorApplication, or FunctionApplication
                        int arity;
                        if (cnFrom.getElement() instanceof ConstructorApplication
                                && cnTo.getElement() instanceof ConstructorApplication) {
                            ConstructorApplication ce1 = (ConstructorApplication) cnFrom.getElement();
                            arity = ce1.getCons().getArity();
                            if (!ce1.getCons()
                                    .equals(((ConstructorApplication) cnTo.getElement()).getCons())) {
                                continue;
                            }
                        } else if (cnFrom.getElement() instanceof FunctionApplication
                                && cnTo.getElement() instanceof FunctionApplication) {
                            FunctionApplication fe1 = (FunctionApplication) cnFrom.getElement();
                            arity = fe1.getFunc().getArity();
                            if (!fe1.getFunc()
                                    .equals(((FunctionApplication) cnTo.getElement()).getFunc())) {
                                continue;
                            }
                        } else {
                            continue;
                        }

                        Application ce1 = (Application) cnFrom.getElement();
                        Application ce2 = (Application) cnTo.getElement();
                        // depending on the variance of the parameters, we need to
                        // infer an edge from cnFrom to cnTo (covariant), cnTo to cnFrom
                        // (contravariant), or both (invariant)
                        boolean ltor = false, rtol = false;
                        if (ce1.getVariance().equals(Variance.POS)) {
                            ltor = true;
                        } else if (ce1.getVariance().equals(Variance.NEG)) {
                            rtol = true;
                        } else if (ce1.getVariance().equals(Variance.NONE)) {
                            ltor = true;
                            rtol = true;
                        }

                        // every parameter must flow (both ways, for invariant parameters)
                        List<List<Option>> components = new ArrayList<>();
                        for (int i = 0; i < arity; i++) {
                            Node n1 = g.getNode(ce1.getElements().get(i));
                            Node n2 = g.getNode(ce2.getElements().get(i));
                            components.add(leqOptions(n1, n2));
                            if (ce1.getVariance().equals(Variance.NONE)) {
                                components.add(leqOptions(n2, n1));
                            }
                        }
                        if (components.stream().anyMatch(List::isEmpty)) {
                            continue;
                        }
                        if (ltor) {
                            inferFromComponents(cnFrom, cnTo, components);
                        }
                        if (rtol) {
                            inferFromComponents(cnTo, cnFrom, components);
                        }
                    }
                }
            }
        }
    }
}
