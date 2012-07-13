package constraint.graph;

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
    
    abstract public String toString ();
    
    abstract public String toDotString ();
    
    abstract public boolean isDirected ();
    
    abstract public void setCause ();
    
    abstract public Environment getAssumption ();
//    LabelEnv getEnv () {
//        if (equ!=null)     
//            return equ.env();
//        else return null;
//    }
}
