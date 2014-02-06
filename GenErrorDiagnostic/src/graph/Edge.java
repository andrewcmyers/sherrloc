package graph;

import constraint.ast.Environment;

abstract public class Edge {
    Node from;
    Node to;
    
    public Edge(Node from, Node to) {
    	this.from = from;
    	this.to = to;
	}
    
    public Node getFrom() {
		return from;
	}
    
    public Node getTo() {
		return to;
	}

	public void incSuccCounter() {
		// do nothing
	}
	
	public void setCause() {
		// do nothing
	}
	    
    abstract public String toString ();
    
    abstract public String toDotString ();
    
    abstract public boolean isDirected ();
    
    abstract public int getLength ();
    
    abstract public Environment getAssumption ();
    
    abstract public Edge getReverse ();
}
