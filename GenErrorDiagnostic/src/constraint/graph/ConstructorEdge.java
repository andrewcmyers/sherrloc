package constraint.graph;

import constraint.ast.Environment;

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
	public String toDotString() {
        return condition.toString(); 
    }
	
	@Override
	public boolean isDirected() {
		return true;
	}
	
	@Override
	public void setCause() {
		// do nothing, fix me later
	}
	
	@Override
	public Environment getAssumption() {
		return new Environment();
	}
}
