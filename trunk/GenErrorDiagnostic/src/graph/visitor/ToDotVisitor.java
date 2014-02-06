package graph.visitor;

import graph.ElementNode;
import graph.Node;

import java.util.HashSet;
import java.util.Set;

/*
 * generate dot file
 */
public class ToDotVisitor implements NodeVisitor {
    Set<Integer> sourcePosition = new HashSet<Integer>();
    String nodes = "";
    String links = "";
    
    public void discoverVertex(Node n) {
        return;
    }
    
    public void leaveVertex(Node n) {
        return;
    }
    
    public void visit(Node node) {
        if (node instanceof ElementNode) {
            ElementNode n = (ElementNode) node;
            if (!n.shouldprint)
                return;
//            sourcePosition.addAll(n.getPositions());
            nodes += n.printNodeToDotString();
            links += n.printLinkToDotString();
        }
    }
    
    public String getNodeString () { return nodes;}
    public String getLinkString () { return links;}
    public Set<Integer> getSourcePosition () { return sourcePosition;}
}
