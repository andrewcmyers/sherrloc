package constraint.graph;

import java.util.List;


/* a special edge corresponds to terminal CONS^-1 */
public class RightEdge extends ReductionEdge {
	public EdgeCondition cons;
	
	public RightEdge(EdgeCondition cons, Node from, Node to, List<Edge> edges) {
		super(from, to, edges);
		this.cons = cons;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof RightEdge) {
			return cons.equals(((RightEdge)obj).cons) && from.equals(((RightEdge)obj).from) && to.equals(((RightEdge)obj).to);
		}
		return false;
	}
	
	public int hashCode() {
		return from.hashCode()*7103+to.hashCode()*3343+cons.hashCode();
	}
	
	public String toString() {
		return "right";
	}
	
	public String toDotString() {
		return "right";
	}
}

