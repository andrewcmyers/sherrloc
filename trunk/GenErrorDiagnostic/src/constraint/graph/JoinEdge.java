package constraint.graph;

import constraint.ast.Environment;

/**
 * A special edge used for the sepcial constructor JOIN (ala. INTERSECTION)
 * The singlton design patten is used to save memory consumption
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
	public Environment getAssumption() {
		return new Environment();
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
