package constraint.graph.visitor;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import constraint.graph.Node;

//find all paths from end nodes to the failed edge. Direction depends on field forward
// This is a DFS search on the graph
public class AllPathFinder implements NodeVisitor {
    ArrayList<Node> currentpath;
    Set<List<Node>> results;
    Node end;       // when end is null, use heuristics
    boolean isbackward;
    
    public AllPathFinder(Set<List<Node>> results, Node end, boolean isBackward) {
        this.results = results;
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
                results.add((List)currentpath.clone());
        }
        else if (end.equals(n))
            results.add((List)currentpath.clone());
    }
}
