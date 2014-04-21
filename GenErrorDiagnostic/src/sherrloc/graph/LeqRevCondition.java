package sherrloc.graph;

/**
 * A singleton class that represents the empty condition on LEQ edges
 */
public class LeqRevCondition extends EdgeCondition {
	private static LeqRevCondition instance = null;
	
	private LeqRevCondition() {
		super(null, 0, false, null);
	}
	
	public static LeqRevCondition getInstance () {
		if (instance==null)
			instance = new LeqRevCondition();
		return instance;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof LeqRevCondition) {
			return true;
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return 3571;
	}
	
	@Override
	public boolean matches (EdgeCondition c) {
		return false;
	}
	
	@Override
	public String toString() {
		return "LEQ_REV";
	}
}