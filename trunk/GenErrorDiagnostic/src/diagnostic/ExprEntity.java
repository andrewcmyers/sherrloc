package diagnostic;

import constraint.graph.ElementNode;

public class ExprEntity extends Entity {
	final private String expr;
	
	public ExprEntity(String expr, int succ) {
		super(succ);
		this.expr = expr;
	}
	
	@Override
	public boolean matches(ElementNode n) {
		return n.toString().equals(expr);
	}
	
	@Override
	public String toString() {
		return expr;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ExprEntity) {
			return expr.equals(((ExprEntity) obj).expr);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return expr.hashCode();
	}
}
