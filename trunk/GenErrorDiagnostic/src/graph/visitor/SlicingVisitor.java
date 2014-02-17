package graph.visitor;

import graph.Node;

//this visitor just labels all node visited as "shouldprint"
public class SlicingVisitor implements NodeVisitor {
    public void discoverVertex(Node n) {
        return;
    }
    
    public void leaveVertex(Node n) {
        return;
    }
    
    public void visit(Node n) {
        if (n.isCause())
            n.markAsPrint();
    }
}
