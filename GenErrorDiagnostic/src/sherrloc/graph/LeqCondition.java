package sherrloc.graph;

/**
 * A singleton class that represents the empty condition on LEQ edges
 */
public class LeqCondition extends EdgeCondition {
	private static LeqCondition instance = null;
	
	private LeqCondition() {
		super(null, 0, false, null);
	}
	
	public static LeqCondition getInstance () {
		if (instance==null)
			instance = new LeqCondition();
		return instance;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof LeqCondition) {
			return true;
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return 1137;
	}
	
	@Override
	public boolean matches (EdgeCondition c) {
		return false;
	}
	
	@Override
	public String toString() {
		return "LEQ";
	}
}