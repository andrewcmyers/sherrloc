package constraint.graph;

import java.util.List;
import java.util.Map;

import constraint.ast.Element;

//a node in label-flow graph has an unique id (to generate dot graph), and a Jif label
public class ElementNode extends Node {
    String uid;
    Element e;
    Graph g;
          
    public ElementNode (String id, Element e, Graph g) {
        super(g);
        this.uid = id;
        this.e = e;
        this.g = g;
    }
    
    // TODO: shall we distinguish pc labels for better explanation?
//    boolean ispc () {
//        return ( (label instanceof JoinLabel) && label.description()!=null && label.description().equals("pc label"));
//    }
            
    public String getName () {
    	return e.toString();
    }
    
//    public Position position () {
//        return e.position();
//    }
    
    /* treat join labels in backtracking specially for better error message */
    public boolean isend () {
    	return e.hasVars();
    }
    
    public String toString() {
        return "Current node: "+getName()+"\n";
    }
    
    public String printNodeToDotString () {
        if (isCause())
            return  uid + " [style=filled, fillcolor=yellow, label=\"" + getName()+ "\"];\n";
//            return  uid + " [style=filled, fillcolor=yellow, label=\"" + getName()+"\\n"+label.position() + "\"];\n";
        else
            return  uid + " [label=\"" + getName()+ "\"];\n";
//            return  uid + " [label=\"" + getName()+"\\n"+label.position() + "\"];\n";
    }
    
    public String printLinkToDotString () {
        String ret = "";
        Map<Node, Edge> neighbors = g.getNeighbors(this);
        for (Node node : neighbors.keySet()) {
            ElementNode n = (ElementNode) node;
            Edge edge = g.getEdge(this, n);
            if (edge instanceof IdEdge)
            	continue;
            if (n.shouldprint) {
            	if (edge.isDirected())
            		ret += this.uid + "->" + n.uid + " [label=\"" + edge.toDotString() + "\"];\n";
            	else if (g.getIndex(this)<g.getIndex(n))
            		ret += this.uid + "->" + n.uid + " [dir=both label=\"" + edge.toDotString() + "\"];\n";
            }
        }
        return ret;
    }

	public String printReductionLinkToDotString() {
		String ret = "";
		Map<Node, List<ReductionEdge>> neighbors = g.getReductionNeighbors(this);
		for (Node node : neighbors.keySet()) {
			ElementNode n = (ElementNode) node;
			for (Edge edge : neighbors.get(n)) {
				if (n.shouldprint) {
					if (edge.isDirected())
						ret += this.uid + "->" + n.uid + " [label=\""
								+ edge.toDotString() + "\"];\n";
					else if (g.getIndex(this) < g.getIndex(n))
						ret += this.uid + "->" + n.uid + " [dir=both label=\""
								+ edge.toDotString() + "\"];\n";
				}
			}
		}
		return ret;
	}
    
    // get the position of current node and out links that links to printable nodes
//    public Set<Integer> getPositions () {
//        Set<Integer> ret = new HashSet<Integer>();
//        if (this.position()!=null)
//            ret.add(this.position().line());
//        for (Node n: outs.keySet()) {
//            if (n.shouldprint)
//                ret.add(((FlowEdge)outs.get(n)).getLineno());
//        }
//        return ret;
//     }
}
