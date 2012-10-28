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
		if (obj instanceof LeftEdge) {
			return cons.equals(((LeftEdge)obj).cons) && from.equals(((LeftEdge)obj).from) && to.equals(((LeftEdge)obj).to);
		}
		return false;
	}
	
	public int hashCode() {
		return from.hashCode()*8009+to.hashCode()*4327+cons.hashCode();
	}
	
	public String toDotString() {
		return "left";
	}
	
	public String toString() {
		return "left";
	}
	
	public EdgeCondition getCondition () {
		return cons;
	}
}
