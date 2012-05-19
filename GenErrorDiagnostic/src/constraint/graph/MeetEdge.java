package constraint.graph;

/**
 * A special edge used for the sepcial constructor MEET (ala. UNION)
 * The singlton design patten is used to save memory consumption
 */
public class MeetEdge extends Edge {
	private static MeetEdge edge=null;
	
	private MeetEdge(Node from, Node to) {
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
}
