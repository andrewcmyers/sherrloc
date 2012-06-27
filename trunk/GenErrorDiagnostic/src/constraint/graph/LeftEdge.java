package constraint.graph;

import java.util.List;


/* a special edge corresponds to nonterminal left */
public class LeftEdge extends ReductionEdge {
	public EdgeCondition cons;
	
	public LeftEdge(EdgeCondition cons, Node from, Node to, List<Edge> edges) {
		super(from, to, edges);
		this.cons = cons;
	}
	
	public boolean equals(Object obj) {
		return (obj instanceof LeftEdge) && cons.equals(((LeftEdge)obj).cons);
	}
	
	public int hashCode() {
		return cons.hashCode();
	}
	
	public String toDotString() {
		return "left";
	}
	
	public String toString() {
		return "left";
	}
}
