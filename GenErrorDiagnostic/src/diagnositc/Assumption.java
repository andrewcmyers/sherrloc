package diagnositc;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import util.Pair;
import constraint.graph.ElementNode;

/* 
 * assumption is a set of source->sink pairs
 * it is simply ranked by size
 * 
 */
public class Assumption implements Comparable<Assumption> {
	Set<Pair<ElementNode, ElementNode>> set;
	
	public Assumption(Stack<Pair<ElementNode, ElementNode>> s) {
		set = new HashSet<Pair<ElementNode, ElementNode>>(s);
	}
	
	public void addPair(Pair<ElementNode, ElementNode> p) {
		set.add(p);
	}
	
	public int getSize () {
		return set.size();
	}
	
	@Override
	public int compareTo(Assumption o) {
		return ((Integer)getSize()).compareTo(o.getSize());
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (Pair<ElementNode, ElementNode> p : set) {
			sb.append(p.getLeft().getElement()+"-->"+p.getRight().getElement()+"; ");
		}
		return sb.toString();
	}
}
