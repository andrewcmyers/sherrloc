package constraint.graph;


abstract public class Node implements Comparable<Node>{
    int count;
    public boolean shouldprint;
    boolean iscause;
          
    public Node(Graph g) {
        shouldprint = false;
        iscause = false;
        count = 0;
        g.addNode(this);
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
    
    public int compareTo(Node n) {
        double rank1 = getRank();
        double rank2 = n.getRank();
        return Double.compare(rank2, rank1);
    }
        
    abstract public void incSuccCounter ();
    abstract public void incNestedCounter (int i);
    abstract public double getSuccCounter ();
}
