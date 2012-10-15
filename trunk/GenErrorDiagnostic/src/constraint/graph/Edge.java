package constraint.graph;

import constraint.ast.Environment;


abstract public class Edge {
    Node from;
    Node to;
    int succPaths = 0;
    
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

    // only equation edges needs the increasement
	public void incSuccCounter() {
		// do nothing
	}
	
	// only equation edges needs the increasement
	public void setCause() {
		// do nothing
	}
    
    abstract public String toString ();
    
    abstract public String toDotString ();
    
    abstract public boolean isDirected ();
    
    abstract public Environment getAssumption ();
//    LabelEnv getEnv () {
//        if (equ!=null)     
//            return equ.env();
//        else return null;
//    }
}
