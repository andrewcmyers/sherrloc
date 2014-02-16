package graph;

import java.util.HashSet;
import java.util.Set;

import constraint.ast.Inequality;

public class EmptyEdge extends Edge {
	private static EmptyEdge instance=null;
	
	private EmptyEdge() {
		super(null, null);
	}
	
	public static EmptyEdge getInstance() {
		if (instance==null)
			instance = new EmptyEdge();
		return instance;
	}


	@Override
	public Set<Inequality> getInequalities() {
		return new HashSet<Inequality>();
	}
	
	@Override
	public int getLength() {
		return 0;
	}

	@Override
	public Edge getReverse() {
		return this;
	}

	@Override
	public boolean isDirected() {
		return false;
	}

	@Override
	public String toDotString() {
		return "";
	}

	@Override
	public String toString() {
		return "";
	}

}
