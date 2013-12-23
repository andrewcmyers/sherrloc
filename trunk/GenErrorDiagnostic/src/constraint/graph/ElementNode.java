package constraint.graph;

import java.util.Map;
import java.util.Set;

import constraint.ast.Element;

//a node in label-flow graph has an unique id (to generate dot graph), and a constraint element
public class ElementNode extends Node {
    String uid;
    Element e;
    Graph g;
    boolean isInCons;
          
    public ElementNode (String id, Element e, Graph g, boolean isIncons) {
        super(g);
        this.uid = id;
        this.e = e;
        this.g = g;
        this.isInCons = isIncons;
    }
            
    public String getName () {
    	return e.toDetailString();
    }
    
    public Element getElement() {
		return e;
	}
    
    public boolean isInCons () {
    	return isInCons;
    }
    
    /* treat join labels in backtracking specially for better error message */
    public boolean isend () {
    	return e.hasVars();
    }
    
    public String toString() {
        return getName();
    }
    
    public void incSuccCounter () {
        e.incSuccCounter(1);
    }
    
    public void incNestedCounter (int i) {
        e.incSuccCounter(i);
    }
    
    public double getSuccCounter () {
		return e.getSuccCounter();
	}
    
    public String printNodeToDotString () {
        if (isCause())
            return  uid + " [style=filled, fillcolor=yellow, label=\"" + e.toDotString()+ "\"];\n";
//            return  uid + " [style=filled, fillcolor=yellow, label=\"" + getName()+"\\n"+label.position() + "\"];\n";
        else
            return  uid + " [label=\"" + e.toDotString()+ "\"];\n";
//            return  uid + " [label=\"" + getName()+"\\n"+label.position() + "\"];\n";
    }
    
    public String printLinkToDotString () {
        String ret = "";
        Map<Node, Set<Edge>> neighbors = g.getNeighbors(this);
        for (Node node : neighbors.keySet()) {
            ElementNode n = (ElementNode) node;
			for (Edge edge : g.getEdges(this, n)) {
				// if (edge instanceof IdEdge)
				// continue;
				if (n.shouldprint) {
					if (edge.isDirected())
						ret += this.uid + "->" + n.uid + " [label=\""
								+ edge.toDotString() + "\"];\n";
					else if (this.getIndex() < n.getIndex())
						ret += this.uid + "->" + n.uid + " [dir=both label=\""
								+ edge.toDotString() + "\"];\n";
				}
			}
        }
        return ret;
    }

}
