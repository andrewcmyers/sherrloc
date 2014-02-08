package graph;

import java.util.Map;
import java.util.Set;

import constraint.ast.Element;

/**
 * A node in the constraint graph represents an element in the constraint
 * language. A node <code>Node</code> has an unique id (to generate dot graph),
 * a constraint element, 
 * 
 */
public class Node {
    private int index; // index in graph
    private Element element;
    private Graph graph;
    private String uid;
    public boolean shouldprint;
    boolean iscause;
          
    public Node(String uid, Element element, Graph graph) {
        shouldprint = false;
        iscause = false;
        graph.addNode(this);
        index = graph.getAllNodes().indexOf(this);
        this.element = element;
        this.graph = graph;
        this.uid = uid;
    }
        
    void setCause () {
        iscause = true;
    }
    
    public boolean isCause () {
        return iscause;
    }
            
    public int getIndex() {
		return index;
	}
    
    public String getName () {
    	return element.toDetailString();
    }
    
    public Element getElement() {
		return element;
	}
            
    public String toString() {
        return getName();
    }
    
    public void incSuccCounter () {
        element.incSuccCounter(1);
    }
    
    public void incNestedCounter (int i) {
        element.incSuccCounter(i);
    }
    
    public int getSuccCounter () {
		return element.getSuccCounter();
	}
    
    public String printNodeToDotString () {
        if (isCause())
            return  uid + " [style=filled, fillcolor=yellow, label=\"" + element.toDotString()+ "\"];\n";
        else
            return  uid + " [label=\"" + element.toDotString()+ "\"];\n";
    }
    
    public String printLinkToDotString () {
        String ret = "";
        Map<Node, Set<Edge>> neighbors = graph.getNeighbors(this);
        for (Node n : neighbors.keySet()) {
			for (Edge edge : graph.getEdges(this, n)) {
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
