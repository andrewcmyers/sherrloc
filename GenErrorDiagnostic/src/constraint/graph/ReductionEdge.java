package constraint.graph;

import java.util.List;

/*
 * Special edges used in CFL-reachability algorithm
 */
public class ReductionEdge extends Edge implements Comparable<ReductionEdge> {
	List<Edge> edges;
	
	public ReductionEdge(Node from, Node to, List<Edge> edges) {
		super(from, to);
		this.edges = edges;
	}
	
	public boolean isDirected() {
		return true;
	}
	
	public String toDotString() {
		return "reduction";
	}
	
	public String toString() {
		return "reduction";
	}
	
	public List<Edge> getEdges() {
		return edges;
	}
	
	public int compareTo(ReductionEdge n) {
		return Double.compare(edges.size(), ((ReductionEdge) n).edges.size());
	}
	
	@Override
	public void setCause() {
		// do nothing
	}
}
