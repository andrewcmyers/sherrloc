package graph;

import constraint.ast.Environment;

/**
 * A special edge used for the special constructor MEET (aka, UNION)
 * The singleton design pattern is used to save memory consumption
 */
public class MeetEdge extends Edge {
	
	public MeetEdge(Node from, Node to) {
		super(from, to);
	}
		
	@Override
	public String toString() {
		return "meet";
	}
	
	@Override
	public String toDotString() {
        return  "meet"; 
    }
	
	@Override
	public boolean isDirected() {
		return true;
	}
	
	@Override
	public Environment getAssumption() {
		return new Environment();
	}
	
	@Override
	public int getLength() {
		return 1;
	}
	
	@Override
	public Edge getReverse() {
		return new MeetEdge(to, from);
	}
}
