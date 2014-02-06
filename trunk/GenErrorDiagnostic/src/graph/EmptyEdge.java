package graph;

import constraint.ast.Environment;

public class EmptyEdge extends Edge {
	static EmptyEdge instance=null;
	
	private EmptyEdge() {
		super(null, null);
	}
	
	public static EmptyEdge getInstance() {
		if (instance==null)
			instance = new EmptyEdge();
		return instance;
	}

	@Override
	public Environment getAssumption() {
		return null;
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
