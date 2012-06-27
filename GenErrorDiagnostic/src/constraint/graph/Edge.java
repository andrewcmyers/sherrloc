package constraint.graph;


abstract public class Edge implements Comparable<Edge>{
	int count;
    boolean iscause = false;
    Node from;
    Node to;
    
    public Edge(Node from, Node to) {
    	this.from = from;
    	this.to = to;
	}
    
    void setCause () {
        iscause = true;
        from.setCause();
        to.setCause();
        count ++;
    }
    
    boolean isCause () {
        return iscause;
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
//    LabelEnv getEnv () {
//        if (equ!=null)     
//            return equ.env();
//        else return null;
//    }
    
    int getCount () {
        return count;
    }
    
    double getRank () {
    	return count;
//        return ((double)count)/((double)totalcount);
    }
    
    public int compareTo(Edge n) {
        double rank1 = getRank();
        double rank2 = n.getRank();
        return Double.compare(rank2, rank1);
    }
}
