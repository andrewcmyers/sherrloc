package sherrloc.graph;

import java.util.HashSet;
import java.util.Set;

import sherrloc.constraint.ast.Inequality;

/**
 * Edges representing meet elements in the constraint language
 */
public class MeetEdge extends Edge {
	
	/**
	 * @param from
	 *            Start node
	 * @param to
	 *            End node
	 */
	public MeetEdge(Node from, Node to) {
		super(from, to);
	}
		
	@Override
	public String toString() {
		return from + " <= " +to;
	}
		
	@Override
	public boolean isDirected() {
		return true;
	}
	
	@Override
	public Set<Inequality> getHypothesis() {
		return new HashSet<Inequality>();
	}
	
	@Override
	public int getLength() {
		return 1;
	}
	
	@Override
	public Edge getReverse() {
		return new MeetEdge(to, from);
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof MeetEdge) {
			return super.equals(obj);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode() + 21737;
	}
}
