package graph;

import java.util.HashSet;
import java.util.Set;

import constraint.ast.Inequality;

/**
 * A special edge used for the special constructor JOIN (aka. INTERSECTION)
 * The singleton design pattern is used to save memory consumption
 */
public class JoinEdge extends Edge {
	
	public JoinEdge(Node from, Node to) {
		super(from, to);
	}
	
	@Override
	public String toString() {
		return "join";
	}
	
	@Override
	public String toDotString() {
        return  "join"; 
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
	public int getLength() {
		return 1;
	}
	
	@Override
	public Edge getReverse() {
		return new JoinEdge(to, from);
	}
}
