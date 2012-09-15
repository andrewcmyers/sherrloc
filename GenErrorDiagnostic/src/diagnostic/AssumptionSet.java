package diagnostic;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

/* 
 * assumption is a set of source->sink pairs
 * it is simply ranked by size
 * 
 */
public class AssumptionSet implements Comparable<AssumptionSet> {
	Set<Assumption> set;
	
	public AssumptionSet(Stack<Assumption> s) {
		set = new HashSet<Assumption>(s);
	}
	
	public int getSize () {
		return set.size();
	}
	
	@Override
	public int compareTo(AssumptionSet o) {
		return ((Integer)getSize()).compareTo(o.getSize());
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Assumption p : set) {
			sb.append(p+"; ");
		}
		return sb.toString();
	}
}
