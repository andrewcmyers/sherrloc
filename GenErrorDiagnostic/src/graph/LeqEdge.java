package graph;

/* 
 * a special edge corresponds to nonterminal id 
 * it keeps track of a path for error reporting
 */
public class LeqEdge extends ReductionEdge {
	
	public LeqEdge(Edge first, Edge second) {
		super(first, second);
	}	
	
	public boolean equals(Object obj) {
		if (obj instanceof LeqEdge) {
			return from.equals(((LeqEdge) obj).from) && to.equals(((LeqEdge) obj).to);
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
	
	@Override
	public Edge getReverse() {
		return new LeqEdge(second.getReverse(), first.getReverse());
	}	
}
