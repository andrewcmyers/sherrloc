package constraint.graph;

import java.util.List;


/* a special edge corresponds to terminal CONS^-1 */
public class RightEdge extends ReductionEdge {
	EdgeCondition cons;
	
	public RightEdge(EdgeCondition cons, Node from, Node to, List<Edge> edges) {
		super(from, to, edges);
		this.cons = cons;
	}
	
	public boolean equals(Object obj) {
		return (obj instanceof RightEdge) && cons.equals(((RightEdge)obj).cons);
	}
	
	public int hashCode() {
		return cons.hashCode();
	}
	
	public String toString() {
		return "right";
	}
	
	public String toDotString() {
		return "right";
	}
}

