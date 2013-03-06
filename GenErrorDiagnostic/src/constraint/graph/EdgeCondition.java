package constraint.graph;

import constraint.ast.Constructor;

public class EdgeCondition {
	Constructor con;
	int index;
	boolean reverse;
	Polarity polarity;
	
	public EdgeCondition(Constructor con, int index, boolean reverse, Polarity pol) {
		this.con = con;
		this.index = index;
		this.reverse = reverse;
		this.polarity = pol;
	}
	
	public boolean isReverse () {
		return reverse;
	}
	
	public Polarity getPolarity() {
		return polarity;
	}
	
	public String toString () {
		if (reverse)
			return con.toString()+"@"+index+"^(-1)";
		else
			return con.toString()+"@"+index;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof EdgeCondition) {
			EdgeCondition ec = (EdgeCondition) obj;
			return con.equals(ec.con) && index == ec.index 
				&& reverse == ec.reverse && polarity == ec.polarity; 
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return con.hashCode() * 1237 + index * 131 + polarity.ordinal()*5 + (reverse?1:0);
	}
	
	/** 
	 * the current condition matches c iff
	 * 1. They have the same constructor
	 * 2. They have the same index
	 * 3. Only one of them is reverse
	 * @param c: the condition to be match
	 * @return
	 */
	public boolean matches (EdgeCondition c) {
		return (con.equals(c.con) && index==c.index && logicXOR(reverse,c.reverse) && polarity==c.polarity);
	}
	
	private boolean logicXOR (boolean a, boolean b) {
		return (!(a&&b) && (a||b));
	}
}
