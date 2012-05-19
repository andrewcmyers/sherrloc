package constraint.graph;

/**
 * A special edge used for the sepcial constructor JOIN (ala. INTERSECTION)
 * The singlton design patten is used to save memory consumption
 */
public class JoinEdge extends Edge {
	private static JoinEdge edge=null;
	
	private JoinEdge(Node from, Node to) {
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
}
