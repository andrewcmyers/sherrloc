package constraint.graph;

import java.util.List;

/* 
 * a special edge corresponds to nonterminal id 
 * it keeps track of a path for error reporting
 */
public class IdEdge extends ReductionEdge {
	
	public IdEdge(Node from, Node to, List<Edge> edges) {
		super(from, to, edges);
	}	
	
	public boolean equals(Object obj) {
		if (obj instanceof IdEdge) {
			return from.equals(((IdEdge) obj).from) && to.equals(((IdEdge) obj).to);
		}
		return false;
	}
	
	public int hashCode() {
		return from.hashCode()*5237 + to.hashCode();
	}
	
	public String toString() {
		return "id";
	}
	
	public String toDotString() {
		return "id";
	}
	
}
