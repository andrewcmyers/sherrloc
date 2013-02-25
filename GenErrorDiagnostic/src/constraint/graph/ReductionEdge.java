package constraint.graph;

import java.util.ArrayList;
import java.util.List;

import constraint.ast.Environment;


/*
 * Special edges used in CFL-reachability algorithm
 */
public class ReductionEdge extends Edge{
	Edge first;
	Edge second;
	int size;
	
	public ReductionEdge(Node from, Node to, Edge first, Edge second) {
		super(from, to);
		this.first = first;
		this.second = second;
		size = (first==null?0:first.getLength())+(second==null?0:second.getLength());
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
		else if (first != null)
			ret.add(first);
		
		if (second instanceof ReductionEdge)
			ret.addAll(((ReductionEdge)second).getEdges());
		else if (second != null)
			ret.add(second);
		return ret;
	}
	
	
	@Override
	public Environment getAssumption() {
		Environment env = new Environment();
		if (first!=null)
			env.addEnv(first.getAssumption());
		if (second!=null)
			env.addEnv(second.getAssumption());
		return env;
	}
}
