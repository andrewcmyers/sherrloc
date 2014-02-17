package graph;

import java.util.HashSet;
import java.util.Set;

import constraint.ast.Inequality;

/**
 * edges used for constructors
 */
public class ConstructorEdge extends Edge {
	EdgeCondition condition;
	
	public ConstructorEdge(EdgeCondition condition, Node from, Node to) {
		super(from, to);
		this.condition = condition;
	}
	
	public EdgeCondition getCondition() {
		return condition;
	}
	
	@Override
	public String toString() {
		return condition.toString();
	}
		
	@Override
	public boolean isDirected() {
		return true;
	}
		
	@Override
	public Set<Inequality> getInequalities() {
		return new HashSet<Inequality>();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ConstructorEdge)
			return condition.equals(((ConstructorEdge)obj).getCondition());
		return super.equals(obj);
	}
	
	@Override
	public int hashCode() {
		return condition.hashCode();
	}
	
	@Override
	public int getLength() {
		return 1;
	}
	
	@Override
	public Edge getReverse() {
		return new ConstructorEdge(condition, to, from);
	}
	
}
