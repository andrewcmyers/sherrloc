package sherrloc.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * This class provide basic functions of a graph
 */
public abstract class Graph {

    protected Set<Node> allNodes;
    protected Map<Node, Map<Node, Edge>> leqEdges;
    protected Map<Node, Set<Node>> leqIn;
    protected Map<Node, Map<Node, Set<ConstructorEdge>>> conEdges;

    protected Graph() {
        allNodes = new HashSet<Node>();
        leqEdges = new HashMap<Node, Map<Node, Edge>>();
        conEdges = new HashMap<Node, Map<Node, Set<ConstructorEdge>>>();
        leqIn = new HashMap<Node, Set<Node>>();
    }

    /**
     * Add node <code>n</code> to graph. No duplicated nodes in graph.
     *
     * @param n Node to be added
     */
    protected void addNode(Node n) {
        allNodes.add(n);
        leqEdges.put(n, new HashMap<Node, Edge>());
        conEdges.put(n, new HashMap<Node, Set<ConstructorEdge>>());
        leqIn.put(n, new HashSet<Node>());
    }

    /**
     * Add edge <code>edge</code> to the graph. End nodes are added as well if they are not
     * represented in graph
     *
     * @param edge Edge to be added
     */
    protected void addLeqEdge(Edge edge) {
        Node from = edge.from;
        Node to = edge.to;
        if (!allNodes.contains(from)) {
            addNode(from);
        }
        if (!allNodes.contains(to)) {
            addNode(to);
        }
        if (!hasLeqEdge(from, to) && !from.equals(to)) {
            leqEdges.get(from).put(to, edge);
            leqIn.get(to).add(from);
        }
    }
    /**
     * Add edge <code>edge</code> to the graph. End nodes are added as well if they are not
     * represented in graph
     *
     * @param edge Edge to be added
     */
    protected void addConEdge(ConstructorEdge edge) {
        Node from = edge.from;
        Node to = edge.to;
        if (!allNodes.contains(from)) {
            addNode(from);
        }
        if (!allNodes.contains(to)) {
            addNode(to);
        }
        if (!hasConEdge(from, to)) {
            conEdges.get(from).put(to, new HashSet<ConstructorEdge>());
        }
        conEdges.get(from).get(to).add(edge);
    }

    /**
     * @return All graph nodes
     */
    public Set<Node> getAllNodes() {
        return allNodes;
    }

    /**
     * @return All edges in the graph
     */
    public List<Edge> getAllEdges() {
        List<Edge> ret = new ArrayList<Edge>();
        for (Node n1 : leqEdges.keySet()) {
            for (Node n2 : leqEdges.get(n1).keySet()) {
                ret.add(leqEdges.get(n1).get(n2));
            }
        }
        for (Node n1 : conEdges.keySet()) {
            for (Node n2 : conEdges.get(n1).keySet()) {
                ret.addAll(conEdges.get(n1).get(n2));
            }
        }
        return ret;
    }

    /**
     * Return true if there is an edge between <code>from</code> and
     * <code>to</code>
     *
     * @param from Start of an edge
     * @param to   End of an edge
     * @return True if there is an edge between <code>from</code> and
     * <code>to</code>
     */
    public boolean hasLeqEdge(Node from, Node to) {
        return leqEdges.get(from).containsKey(to);
    }

    /**
     * Get all edges between <code>from</code> and <code>to</code>
     *
     * @param from Start node
     * @param to   End node
     * @return All edges between <code>from</code> and <code>to</code>
     */
    public Edge getLeqEdge(Node from, Node to) {
        return leqEdges.get(from).get(to);
    }

    /**
     * Return true if there is an edge between <code>from</code> and
     * <code>to</code>
     *
     * @param from Start of an edge
     * @param to   End of an edge
     * @return True if there is an edge between <code>from</code> and
     * <code>to</code>
     */
    public boolean hasConEdge(Node from, Node to) {
        return conEdges.get(from).containsKey(to);
    }

    /**
     * Get all edges between <code>from</code> and <code>to</code>
     *
     * @param from Start node
     * @param to   End node
     * @return All edges between <code>from</code> and <code>to</code>
     */
    public List<Edge> getEdges(Node from, Node to) {
        List<Edge> ret = new ArrayList<Edge>();
        if (hasLeqEdge(from, to)) {
            ret.add(leqEdges.get(from).get(to));
        }
        if (hasConEdge(from, to)) {
            ret.addAll(conEdges.get(from).get(to));
        }
        return ret;
    }

    /**
     * Return all adjacent nodes of <code>from></code>. Useful for outputting the graph as a DOT
     * file
     *
     * @param from
     * @return All adjacent nodes of <code>from></code>
     */
    public Set<Node> getNeighbors(Node from) {
        Set<Node> ret = new HashSet<Node>();
        ret.addAll(leqEdges.get(from).keySet());
        ret.addAll(conEdges.get(from).keySet());
        return ret;
    }

    /**
     * Label the entire graph as printable for the DOT format output
     */
    public void labelAll() {
        for (Node n : allNodes) {
            n.markAsPrint();
        }
    }
}
