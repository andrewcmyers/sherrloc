package constraint.graph.visitor;

import java.util.ArrayList;
import java.util.List;

import constraint.graph.Node;

//find all paths from end nodes to the failed edge. Direction depends on field forward
// This is a DFS search on the graph
public class DFSPathVisitor implements NodeVisitor {
    ArrayList<Node> currentpath;
    List<Node> results;
    Node end;       // when end is null, use heuristics
    boolean isbackward;
    
    public DFSPathVisitor(Node end, boolean isBackward) {
        currentpath = new ArrayList<Node>();
        this.isbackward = isBackward;
        this.end = end;
    }
    
    public void discoverVertex(Node n) {
        if (isbackward)
            currentpath.add(0, n);
        else
            currentpath.add(n);
    }
    
    public void leaveVertex(Node n) {
        if (isbackward)
            currentpath.remove(0);
        else
            currentpath.remove(currentpath.size()-1);
    }
    
    public void visit(Node n) {
        if (end==null) {
            if (n.isend())
                results = (List) currentpath.clone();
        }
        else if (end.equals(n))
            results = (List) currentpath.clone();
    }
    
    public List<Node> getResults () {
    	return results;
    }
}
