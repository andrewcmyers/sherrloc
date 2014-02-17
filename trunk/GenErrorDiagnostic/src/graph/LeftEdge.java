package graph;

/* a special edge corresponds to nonterminal left */
public class LeftEdge extends ReductionEdge {
	public EdgeCondition cons;
	
	public LeftEdge(EdgeCondition cons, Edge first, Edge second) {
		super(first, second);
		this.cons = cons;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof LeftEdge) {
			LeftEdge le = (LeftEdge)obj;
			return cons.equals(le.cons) && from.equals(le.from) && to.equals(le.to);
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
	
	@Override
	public Edge getReverse() {
		return new LeftEdge(cons, second.getReverse(), first.getReverse());
	}
}
