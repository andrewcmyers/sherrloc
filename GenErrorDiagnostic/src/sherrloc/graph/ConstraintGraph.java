package sherrloc.graph;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import sherrloc.constraint.ast.Application;
import sherrloc.constraint.ast.Axiom;
import sherrloc.constraint.ast.Constraint;
import sherrloc.constraint.ast.ConstructorApplication;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.EnumerableElement;
import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.ast.Inequality;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.Position;
import sherrloc.constraint.ast.Relation;
import sherrloc.constraint.ast.Variable;
import sherrloc.constraint.ast.VariableApplication;
import sherrloc.util.StringUtil;

/**
 * A constraint graph is built on a set of constructors (including user-defined ones), assumptions,
 * as well as constraints as inputs.
 * <p>
 * This class builds a basic graph from the constraints, without saturation. Different
 * <code>PathFinders</code> are used to saturate the graph, which is later used as inputs to infer
 * the most likely error cause.
 */
public class ConstraintGraph extends Graph {

    private Hypothesis env;
    private List<Axiom> rules;

    private Set<String> files;                                          // source codes involved, only used for DOT files
    private final boolean PRINT_SRC = false;                                // print corresponding source code in DOT files
    private Map<Element, Node> eleToNode = new HashMap<Element, Node>(); // map from AST elements to graph nodes
    private Map<Integer, Node> idxToNode = new HashMap<Integer, Node>(); // map from integers to graph nodes
    private int varCounter = 0;
    private boolean isSymmetric = true;

    /**
     * Optimizations
     */
    private boolean OPT_AXIOMS = true;
    // A map from base elements (elements with no position info) to potentially multiple uses of the element
    // Useful for matching axioms in a graph.
    private Map<Element, List<Node>> baseToNodes = new HashMap<Element, List<Node>>();

    /**
     * @param env         Global assumptions
     * @param constraints Constraints
     */
    public ConstraintGraph(Hypothesis env, Set<Constraint> constraints, List<Axiom> axioms) {
        this(env, axioms);
        /**
         * generate the simple links from the constraints. handle constructors,
         * meet and join later
         */
        for (Constraint cons : constraints) {
            addOneConstraint(cons);
        }
    }

    /**
     * See {@link #ConstraintGraph(Hypothesis, Set)}
     */
    public ConstraintGraph(Hypothesis env, List<Axiom> axioms) {
        this.env = env;
        this.files = new HashSet<String>();
        this.rules = axioms;
    }

    /**
     * Lookup a node representing element <code>e</code> in graph. Create a fresh node if no such
     * node exists
     *
     * @param e Element to find
     * @return A node representing <code>e</code>
     */
    public Node getNode(Element e) {
        return getNode(e, false);
    }

    /**
     * Lookup a node representing element <code>e</code> in graph. Create a fresh node if no such
     * node exists
     *
     * @param e      Element to find
     * @param isGray True if the node is created during graph saturation
     * @return A node representing <code>e</code>
     */
    public Node getNode(Element e, boolean isGray) {
        if (!eleToNode.containsKey(e)) {
            Node n = new Node(varCounter, e, isGray);
            addNode(n);
            varCounter++;
            eleToNode.put(e, n);
            idxToNode.put(n.getIndex(), n);
            Element baseEle = e.getBaseElement();
            if (!baseToNodes.containsKey(baseEle)) {
                baseToNodes.put(baseEle, new ArrayList<Node>());
            }
            baseToNodes.get(baseEle).add(n);
        }
        return eleToNode.get(e);
    }

    /**
     * Return a node with index idx. Return null if no such node exists
     *
     * @param idx Index of a node to be found
     * @return The node with index idx. Null if no such node exists
     */
    public Node getNode(int idx) {
        return idxToNode.get(idx);
    }

    /**
     * @param e A constraint element
     * @return True if the graph has a node representing <code>e</code>
     */
    public boolean hasElement(Element e) {
        return eleToNode.containsKey(e);
    }

    /**
     * @return A list of elements that represent baseElement. Return null if no such element exists.
     */
    public List<Node> getMatchedNodes(Element baseElement) {
        return baseToNodes.get(baseElement);
    }

    /**
     * Adding a constraint to graph (add edges between nodes representing constraint elements)
     *
     * @param cons Constraint
     */
    public void addOneConstraint(Constraint cons) {
        Node source = getNode(cons.getFirstElement());
        Node to = getNode(cons.getSecondElement());

        addLeqEdge(new ConstraintEdge(cons, source, to));

        if (cons.getRelation() == Relation.EQ) {
            addLeqEdge(new ConstraintEdge(cons, to, source));
        } else {
            isSymmetric = false;
        }
    }

    /**
     * Adding an inequality to graph
     *
     * @param ieq Inequality to be added
     */
    public void addOneInequality(Inequality ieq) {
        addOneConstraint(new Constraint(ieq, null, Position.EmptyPosition()));
    }

    /**
     * Add a list of implication rules
     *
     * @param lst A list of implication rules to be added
     */
    public void addRules(List<Axiom> lst) {
        rules.addAll(lst);
    }

    /**
     * Add a one implication rule
     *
     * @param lst A list of implication rules to be added
     */
    public void addOneRule(Axiom a) {
        rules.add(a);
    }

    /**
     * @return A list of implication rules
     */
    public List<Axiom> getRules() {
        return rules;
    }

    /**
     * Generate a constraint graph from constraints
     */
    public void generateGraph() {
//        if (generated)
//            return;

        /**
         * generate extra nodes and edges for constructors, join and meet elements
         * 1. Constructor edges between a constructor and its parameters
         * 2. Edges from components to a join element
         * 3. Edges from a meet element to components
         */
        List<Element> workingList = new ArrayList<Element>(eleToNode.keySet());
        Set<Element> processed = new HashSet<Element>();

        while (workingList.size() != 0) {
            Element e = workingList.get(0);
            Node currentnode = getNode(e);
            workingList.remove(0);
            processed.add(e);

            // generate the source node
            Collection<Element> compset;

            if (e instanceof EnumerableElement) {
                EnumerableElement ee = (EnumerableElement) e;
                compset = ee.getElements();

                int index = 0;
                for (Element element : compset) {
                    Node compnode = getNode(element);
                    index++;
                    // add the component element to the working list if not seen before
                    if (!processed.contains(element) && !workingList.contains(element)) {
                        workingList.add(element);
                    }

                    if (e instanceof MeetElement) {
                        addLeqEdge(new MeetEdge(currentnode, compnode));
                    } else if (e instanceof JoinElement) {
                        addLeqEdge(new JoinEdge(compnode, currentnode));
                    } else if (e instanceof Application) {
                        Application ae = (Application) e;
                        Variance variance = ae.getVariance();
                        if (ae instanceof ConstructorApplication && !ae.getVariance()
                                .equals(Variance.NONE)) {
                            addConEdge(new ConstructorEdge(
                                    new EdgeCondition(((ConstructorApplication) ae).getCons(),
                                            index, false, variance), compnode, currentnode));
                            addConEdge(new ConstructorEdge(
                                    new EdgeCondition(((ConstructorApplication) ae).getCons(),
                                            index, true, variance), currentnode, compnode));
                        } else if (ae instanceof VariableApplication && !ae.getVariance()
                                .equals(Variance.NONE)) {
                            addConEdge(new ConstructorEdge(
                                    new EdgeCondition(((VariableApplication) ae).getCons(), index,
                                            false, variance), compnode, currentnode));
                            addConEdge(new ConstructorEdge(
                                    new EdgeCondition(((VariableApplication) ae).getCons(), index,
                                            true, variance), currentnode, compnode));
                        }
                    }
                }
            }
        }
        if (OPT_AXIOMS) {
            List<Axiom> useless = new ArrayList<Axiom>();
            for (Axiom rule : rules) {
                for (Inequality ieq : rule.getConclusion()) {
                    boolean used = true;
                    if (!ieq.getFirstElement().hasVars() && !ieq.getFirstElement().hasQVars()) {
                        used = false;
                        for (Node n : allNodes) {
                            if (n.getElement().getBaseElement().equals(ieq.getFirstElement())) {
                                used = true;
                                break;
                            }
                        }
                    }
                    if (used == false) {
                        useless.add(rule);
                        break;
                    }
                    if (!ieq.getSecondElement().hasVars() && !ieq.getSecondElement().hasQVars()) {
                        used = false;
                        for (Node n : allNodes) {
                            if (n.getElement().getBaseElement().equals(ieq.getSecondElement())) {
                                used = true;
                                break;
                            }
                        }
                    }
                    if (used == false) {
                        useless.add(rule);
                        break;
                    }
                }
            }
            rules.removeAll(useless);
            if (env != null) {
                env.setAxioms(rules);
            }
        }

        // add base elements to the hypothesis graph
        if (env != null) {
            env.addElements(getAllElements());
        }
    }

    /**
     * Links from node to all neighbors in graph in DOT format
     *
     * @param node A graph node
     * @return A DOT string representing the links from <code>node</code> to all neighbors in graph
     */
    private String printLinkToDotString(Node node) {
        String ret = "";
        Set<Node> neighbors = getNeighbors(node);
        for (Node n : neighbors) {
            for (Edge edge : getEdges(node, n)) {
                if (n.shouldPrint()) {
                    if (edge.isDirected()) {
                        ret += node.getUid() + "->" + n.getUid() + " [label=\""
                                + edge.toDotStringClean() + "\"];\n";
                    } else if (node.getIndex() < n.getIndex()) {
                        ret += node.getUid() + "->" + n.getUid() + " [dir=both label=\""
                                + edge.toDotStringClean() + "\"];\n";
                    }
                }
            }
        }
        return ret;
    }

    /**
     * @return A string in DOT file format which represents the graph
     */
    public String toDotString() {
        String ret = "";
        String nodes = "";
        String links = "";

        for (Node n : allNodes) {
            if (!n.shouldPrint()) {
                continue;
            }
            nodes += n.toDotString();
            links += printLinkToDotString(n);
        }

        ret += "digraph G1 {\n";
        // print source code
        if (PRINT_SRC) {
            for (String s : files) {
                try {
                    BufferedReader reader = new BufferedReader(new FileReader(s));
                    String line = reader.readLine();
                    int linenum = 1;
                    ret += "source [shape=box, label=\"";
                    while (line != null) {
                        ret += linenum + ":\t" + StringUtil.sanitize(line) + "\\l";
                        line = reader.readLine();
                        linenum++;
                    }
                    ret += "\"];\n";
                } catch (IOException e) {
                    continue;
                }
            }
        }

        ret += "node [color = grey, style = filled];\n";
        ret += nodes;
        ret += links;
        ret += "}\n";
        return ret;
    }

    /**
     * Mark graph nodes that relate to errors
     */
    public void slicing() {
        for (Node node : allNodes) {
            if (node.isCause()) {
                node.markAsPrint();
            }
        }
    }

    /**
     * @return All constraint elements
     */
    public Set<Element> getAllElements() {
        return eleToNode.keySet();
    }

    /**
     * @return Global assumptions
     */
    public Hypothesis getEnv() {
        return env;
    }

    /**
     * @return True if all constraints are symmetric (only equalities)
     */
    public boolean isSymmetric() {
        return isSymmetric;
    }

    /**
     * Removes a node `n` by splicing all edges going in/out of it. Assumes `n` is droppable.
     * @param n
     */
    private void removeNode(Node n) {
        Set<Node> preds = new HashSet<>(leqIn.get(n));
        Map<Node, Edge> outgoingMap = leqEdges.get(n);
        Set<Node> succs = new HashSet<>(outgoingMap.keySet());

        List<Edge> inEdges = new ArrayList<>(preds.size());
        for (Node from : preds) {
            Edge in = leqEdges.get(from).get(n);
            if (in != null) {
                inEdges.add(in);
            }
        }
        List<Edge> outEdges = new ArrayList<>(outgoingMap.values());

        allNodes.remove(n);
        for (Node from : preds) {
            leqEdges.get(from).remove(n);
        }
        for (Node to : succs) {
            Set<Node> toPreds = leqIn.get(to);
            if (toPreds != null) {
                toPreds.remove(n);
            }
        }
        leqEdges.remove(n);
        conEdges.remove(n);
        leqIn.remove(n);

        for (Edge first : inEdges) {
            Node from = first.getFrom();
            for (Edge second : outEdges) {
                Node to = second.getTo();
                if (from.equals(to)) {
                    continue;
                }
                if (hasLeqEdge(from, to)) {
                    continue;
                }
                ConstraintEdge spliced = buildSplicedEdge(first, second, from, to);
                leqEdges.get(from).put(to, spliced);
                leqIn.get(to).add(from);
            }
        }
    }

    /**
     * Build a {@link ConstraintEdge} that stands in for the composition of {@code first} and
     * {@code second}, merging any assumption hypotheses along the way. Used by
     * {@link #removeNode}.
     */

    /**
     * Combines two edges, `first.from = from -> first.to = second.from` and
     * `second.from -> to = second.to` into one edge `from -> to`.
     */
    private ConstraintEdge buildSplicedEdge(Edge first, Edge second, Node from, Node to) {
        Relation r = Relation.LEQ;
        Hypothesis h = new Hypothesis();
        if (first instanceof ConstraintEdge && second instanceof ConstraintEdge) {
            Constraint fcons = ((ConstraintEdge) first).getConstraint();
            Constraint scons = ((ConstraintEdge) second).getConstraint();
            if (fcons.getRelation() == scons.getRelation()) {
                r = fcons.getRelation();
            }
            if (fcons.getAssumption() != null) {
                h.addEnv(fcons.getAssumption());
            }
            if (scons.getAssumption() != null) {
                h.addEnv(scons.getAssumption());
            }
        } else if (first instanceof ConstraintEdge) {
            Constraint fcons = ((ConstraintEdge) first).getConstraint();
            if (fcons.getAssumption() != null) {
                h.addEnv(fcons.getAssumption());
            }
        } else if (second instanceof ConstraintEdge) {
            Constraint scons = ((ConstraintEdge) second).getConstraint();
            if (scons.getAssumption() != null) {
                h.addEnv(scons.getAssumption());
            }
        }
        Constraint c = new Constraint(from.getElement(), to.getElement(), r, h,
                Position.EmptyPosition());
        return new ConstraintEdge(c, from, to);
    }

    /**
     * Returns true if `n` is a trivial node which is droppable. This is the case when
     * - its element is trivial and can be unified with anything (but is not a Join or Meet)
     * - it has no constructor edges
     * - it is not a component of a join or meet element
     */
    private boolean isDroppableTrivial(Node n) {
        Element e = n.getElement();
        return e.trivialEnd()
                && !(e instanceof JoinElement)
                && !(e instanceof MeetElement)
                && conEdges.get(n).isEmpty()
                && !isJoinOrMeetComponent(n);
    }

    private boolean isJoinOrMeetComponent(Node n) {
        for (Edge e : leqEdges.get(n).values()) {
            if (e instanceof JoinEdge || e instanceof MeetEdge) {
                return true;
            }
        }
        for (Node pred : leqIn.get(n)) {
            Edge e = leqEdges.get(pred).get(n);
            if (e instanceof JoinEdge || e instanceof MeetEdge) {
                return true;
            }
        }
        return false;
    }

    /**
     * Reduces the constraint graph in-place while preserving satisfiability. This is done in three
     * phases:
     * 1. Collapse any strongly-connected components of just variables
     * 2. Remove any trivially removable nodes
     * 3. Remove any disconnected nodes that would not provide insight
     */
    public void reduce() {
        collapseLeqSCCs();
        Set<Node> visitedByBfs = pruneTrivialLeaves();
        pruneOrphans(visitedByBfs);
    }

    /**
     * Collapse strongly-connected components of leq edges which contain only variable nodes.
     */
    private void collapseLeqSCCs() {
        List<Set<Node>> sccs = computeLeqSCCs();
        for (Set<Node> scc : sccs) {
            if (scc.size() < 2) {
                continue;
            }
            if (!isVariableOnlyLeqScc(scc)) {
                continue;
            }
            Node rep = pickSccRepresentative(scc);
            for (Node member : new ArrayList<>(scc)) {
                if (member != rep) {
                    mergeNodeInto(rep, member, scc);
                }
            }
        }
    }

    /**
     * Uses Tarjan's algorithm to find strongly-connected components of the leq graph.
     */
    private List<Set<Node>> computeLeqSCCs() {
        Map<Node, Integer> index = new HashMap<>();
        Map<Node, Integer> lowlink = new HashMap<>();
        Set<Node> onStack = new HashSet<>();
        Deque<Node> stack = new ArrayDeque<>();
        int[] counter = {0};
        List<Set<Node>> sccs = new ArrayList<>();

        for (Node v : new ArrayList<>(allNodes)) {
            if (!index.containsKey(v)) {
                strongConnect(v, index, lowlink, onStack, stack, counter, sccs);
            }
        }
        return sccs;
    }

    private void strongConnect(Node v, Map<Node, Integer> index, Map<Node, Integer> lowlink,
            Set<Node> onStack, Deque<Node> stack, int[] counter, List<Set<Node>> sccs) {
        int vIndex = counter[0]++;
        index.put(v, vIndex);
        lowlink.put(v, vIndex);
        stack.push(v);
        onStack.add(v);

        for (Node w : new ArrayList<>(leqEdges.get(v).keySet())) {
            if (!index.containsKey(w)) {
                strongConnect(w, index, lowlink, onStack, stack, counter, sccs);
                lowlink.put(v, Math.min(lowlink.get(v), lowlink.get(w)));
            } else if (onStack.contains(w)) {
                lowlink.put(v, Math.min(lowlink.get(v), index.get(w)));
            }
        }

        if (lowlink.get(v).equals(index.get(v))) {
            Set<Node> scc = new HashSet<>();
            Node w;
            do {
                w = stack.pop();
                onStack.remove(w);
                scc.add(w);
            } while (w != v);
            sccs.add(scc);
        }
    }

    /**
     * True if this strongly-connected set of nodes `scc` is collapsable. This is the case when it
     * contains only variable nodes without outgoing constructor edges.
     */
    private boolean isVariableOnlyLeqScc(Set<Node> scc) {
        for (Node n : scc) {
            if (!(n.getElement() instanceof Variable)) {
                return false;
            }
            if (!conEdges.get(n).isEmpty()) {
                return false;
            }
        }
        return true;
    }

    private Node pickSccRepresentative(Set<Node> scc) {
        Node best = null;
        for (Node n : scc) {
            if (best == null || n.getIndex() < best.getIndex()) {
                best = n;
            }
        }
        return best;
    }

    /**
     * Merge `rep` into `member`, deleting any edges with an opposite edge in `sccMembers`.
     */
    private void mergeNodeInto(Node rep, Node member, Set<Node> sccMembers) {
        for (Node pred : new ArrayList<>(leqIn.get(member))) {
            Edge edge = leqEdges.get(pred).remove(member);
            leqIn.get(member).remove(pred);
            if (edge == null) {
                continue;
            }
            if (sccMembers.contains(pred) || pred.equals(rep)) {
                continue;
            }
            if (leqEdges.get(pred).containsKey(rep)) {
                continue;
            }
            edge.to = rep;
            leqEdges.get(pred).put(rep, edge);
            leqIn.get(rep).add(pred);
        }

        for (Node succ : new ArrayList<>(leqEdges.get(member).keySet())) {
            Edge edge = leqEdges.get(member).remove(succ);
            Set<Node> succPreds = leqIn.get(succ);
            if (succPreds != null) {
                succPreds.remove(member);
            }
            if (edge == null) {
                continue;
            }
            if (sccMembers.contains(succ) || succ.equals(rep)) {
                continue;
            }
            if (leqEdges.get(rep).containsKey(succ)) {
                continue;
            }
            edge.from = rep;
            leqEdges.get(rep).put(succ, edge);
            if (succPreds != null) {
                succPreds.add(rep);
            }
        }

        for (Map.Entry<Element, Node> entry : eleToNode.entrySet()) {
            if (entry.getValue() == member) {
                entry.setValue(rep);
            }
        }
        for (List<Node> nodes : baseToNodes.values()) {
            for (int i = 0; i < nodes.size(); i++) {
                if (nodes.get(i) == member) {
                    nodes.set(i, rep);
                }
            }
        }
        idxToNode.remove(member.getIndex());

        allNodes.remove(member);
        leqEdges.remove(member);
        conEdges.remove(member);
        leqIn.remove(member);
    }

    /**
     * BFS from non-trivial nodes dropping all trivial nodes.
     */
    private Set<Node> pruneTrivialLeaves() {
        Queue<Node> queue = new LinkedList<>();
        Set<Node> queued = new HashSet<>();
        Set<Node> visited = new HashSet<>();

        for (Node n : allNodes) {
            if (!n.getElement().trivialEnd()) {
                queue.add(n);
                queued.add(n);
            }
        }

        while (!queue.isEmpty()) {
            Node curr = queue.remove();
            queued.remove(curr);

            for (Node neighbor : getNeighbors(curr)) {
                if (visited.contains(neighbor)) {
                    continue;
                }
                visited.add(neighbor);
                if (isDroppableTrivial(neighbor)) {
                    removeNode(neighbor);
                    if (!queued.contains(curr)) {
                        queue.add(curr);
                        queued.add(curr);
                    }
                } else {
                    queue.add(neighbor);
                    queued.add(neighbor);
                }
            }
            visited.add(curr);
        }
        return visited;
    }

    /**
     * Remove nodes unreached by the previous BFS stage or any node that is completely disconnected
     * from the rest of the graph. These nodes are not reachable from any information-providing
     * edges, so do not generate any interesting constraints and thus can be removed.
     */
    private void pruneOrphans(Set<Node> visitedByBfs) {
        Set<Node> toRemove = new HashSet<>();
        for (Node n : allNodes) {
            boolean hasLeqOut = !leqEdges.get(n).isEmpty();
            boolean hasCon = !conEdges.get(n).isEmpty();
            boolean hasLeqIn = !leqIn.get(n).isEmpty();

            if (isJoinOrMeetComponent(n)) {
                continue;
            }

            if (!hasLeqOut && !hasCon && !hasLeqIn) {
                toRemove.add(n);
            } else if (!visitedByBfs.contains(n) && hasLeqIn) {
                toRemove.add(n);
            }
        }

        for (Node n : toRemove) {
            allNodes.remove(n);
            for (Node pred : leqIn.get(n)) {
                Map<Node, Edge> predOut = leqEdges.get(pred);
                if (predOut != null) {
                    predOut.remove(n);
                }
            }
            leqEdges.remove(n);
            conEdges.remove(n);
            leqIn.remove(n);
        }
    }
}
