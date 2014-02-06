package graph;

import constraint.ast.Element;


abstract public class Node implements Comparable<Node>{
    int count;
    int index; // index in graph
    public boolean shouldprint;
    boolean iscause;
          
    public Node(Graph g) {
        shouldprint = false;
        iscause = false;
        count = 0;
        g.addNode(this);
        index = g.getAllNodes().indexOf(this);
    }
        
    void setCause () {
        iscause = true;
        count ++;
    }
    
    public boolean isCause () {
        return iscause;
    }
    
    int getCount () {
        return count;
    }
    
    
    double getRank () {
    	return count;
    }
    
    public int getIndex() {
		return index;
	}
    
    public int compareTo(Node n) {
        double rank1 = getRank();
        double rank2 = n.getRank();
        return Double.compare(rank2, rank1);
    }
        
    abstract public void incSuccCounter ();
    abstract public void incNestedCounter (int i);
    abstract public int getSuccCounter ();
    abstract public Element getElement ();
    abstract public String getName();
}
