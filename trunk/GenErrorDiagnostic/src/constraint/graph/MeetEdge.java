package constraint.graph;

import constraint.ast.Environment;

/**
 * A special edge used for the sepcial constructor MEET (ala. UNION)
 * The singlton design patten is used to save memory consumption
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
}
