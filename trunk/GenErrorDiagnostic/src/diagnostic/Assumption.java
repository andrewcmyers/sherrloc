package diagnostic;

import graph.Node;

/* 
 * assumption is a set of source->sink pairs
 * it is simply ranked by size
 * 
 */
public class Assumption {
	Node source;
	Node sink;
	
	public Assumption(Node src, Node snk) {
		source = src;
		sink = snk;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof Assumption)
			return ((Assumption) obj).source.equals(source) && ((Assumption) obj).sink.equals(sink);
		else
			return false;
	}
	
	@Override
	public int hashCode() {
		return source.hashCode() + sink.hashCode();
	}
	
	@Override
	public String toString() {
		return source.getElement() + " <= "+sink.getElement();
	}
}
