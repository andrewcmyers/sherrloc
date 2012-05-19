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
		return (obj instanceof IdEdge);
	}
	
	public int hashCode() {
		return 89;
	}
	
	public String toString() {
		return "id";
	}
	
	public String toDotString() {
		return "id";
	}
	
}
