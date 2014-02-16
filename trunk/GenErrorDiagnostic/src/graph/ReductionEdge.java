package graph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.ast.Inequality;


/*
 * Special edges used in CFL-reachability algorithm
 */
abstract public class ReductionEdge extends Edge{
	Edge first;
	Edge second;
	public int size;
	
	public ReductionEdge(Edge first, Edge second) {
		super(first.from, second.to);
		if (first instanceof EmptyEdge)
			from = second.from;
		if (second instanceof EmptyEdge)
			to = first.to;
		this.first = first;
		this.second = second;
		size = first.getLength()+second.getLength();
	}
	
	public boolean isDirected() {
		return true;
	}
	
	public String toDotString() {
		return "reduction";
	}
	
	public String toString() {
		return "reduction";
	}
	
	public Edge getFirst() {
		return first;
	}
	
	public Edge getSecond(){
		return second;
	}
	
	public int getLength () {
		return size;
	}
	
	public List<Edge> getEdges() {
		List<Edge> ret = new ArrayList<Edge>();
		if (first instanceof ReductionEdge)
			ret.addAll(((ReductionEdge)first).getEdges());
		else if (!(first instanceof EmptyEdge))
			ret.add(first);
		
		if (second instanceof ReductionEdge)
			ret.addAll(((ReductionEdge)second).getEdges());
		else if (!(second instanceof EmptyEdge))
			ret.add(second);
		return ret;
	}
	
	
	@Override
	public Set<Inequality> getInequalities() {
		Set<Inequality> ret = new HashSet<Inequality>();
		ret.addAll(first.getInequalities());
		ret.addAll(second.getInequalities());
		return ret;
	}
}
