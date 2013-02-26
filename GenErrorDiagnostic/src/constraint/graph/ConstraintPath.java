package constraint.graph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.ast.Environment;
import constraint.graph.pathfinder.PathFinder;

public class ConstraintPath {
    List<Edge> edges;
    Environment assumption;
    PathFinder finder;
    
    public ConstraintPath(List<Edge> edges, PathFinder finder) {
        this.edges = edges;
        this.assumption = new Environment();
        for (Edge edge : edges) {
        	assumption.addEnv(edge.getAssumption());
        }
        this.finder =  finder;
    }

    int size () {
        return edges.size();
    }
    
    public List<Edge> getEdges() {
		return edges;
	}
    
	public List<Node> getIdNodes( ) {
		ArrayList<Node> ret = new ArrayList<Node>();
		
		if (edges.size()==0) return ret;

		// System.out.println("Checking one equation in env: "+path.env);
		Node first = getFirst();
		ret.add(first);

		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			if (finder.getPath(first, edge.to)!=null)
				ret.add(edge.to);
		}
		return ret;
	}
	
	public List<Node> getAllNodes( ) {
		ArrayList<Node> ret = new ArrayList<Node>();
		
		if (edges.size()==0) return ret;

		// System.out.println("Checking one equation in env: "+path.env);
		Node first = getFirst();
		ret.add(first);

		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret.add(edge.to);
		}
		return ret;
	}
    
	public Node getFirst() {
		if (edges.size() != 0)
			return edges.get(0).from;
		else
			return null;
	}
    
	public Node getLast() {
		if (edges.size() != 0)
			return edges.get(edges.size()-1).to;
		else
			return null;
	}
	
	public Environment getAssumption () {
		return assumption;
	}
	
	public void incSuccCounter ( ) {
		if (edges.size()==0) return;
		// avoid duplicate expressions
		Set<String> processed = new HashSet<String>();

		ElementNode leftmost = (ElementNode) getFirst();
		leftmost.incSuccCounter();
		processed.add(leftmost.toString());
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.incSuccCounter();
			if (!processed.contains(edge.getTo().toString())) {
				edge.getTo().incSuccCounter();
				processed.add(edge.getTo().toString());
			}
		}
	}
	
	// increase the unlikelihood for constructor nodes
	public void incFailCounter ( ) {
//		if (edges.size()==0) return;
//		// avoid duplicate expressions
//		Set<String> processed = new HashSet<String>();
//
//		ElementNode leftmost = (ElementNode) getFirst();
//		processed.add(leftmost.toString());
//		int curriedLevel = 0;
//		boolean ltor = true;
//		for (int k = 0; k < size(); k++) {
//			Edge edge = edges.get(k);
//			edge.incSuccCounter();
//			if (!processed.contains(edge.getTo().toString())) {
//				if (edge instanceof ConstructorEdge) {
//					ConstructorEdge ce = (ConstructorEdge) edge;
//					if (!ce.getCondition().isReverse() && ltor)
//						curriedLevel++;
//					else if (curriedLevel!=0)
//						curriedLevel--;
//					else {
//						ltor = !ltor;
//						curriedLevel ++;
//					}
//				}
//				edge.getTo().incNestedCounter(curriedLevel);
//				processed.add(edge.getTo().toString());
//			}
//		}
	}
	
	public void setCause ( ) {
		if (edges.size()==0) return;

		ElementNode leftmost = (ElementNode) getFirst();
		leftmost.setCause();
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
		}
	}
	
	public boolean intersects (ConstraintPath path) {
		// check if there are common "expressions"
	    List<Node> nodes1 = getAllNodes();
	    List<Node> nodes2 = path.getAllNodes();
	    for (Node n1 : nodes1) {
	    	for (Node n2 : nodes2) {
    			if (((ElementNode)n1).isInCons() && ((ElementNode)n2).isInCons()) {
    				if (n1.toString().equals(n2.toString())) {
    					return true;
    				}
    			}
	    	}
	    }
	    return false;
	}
    
	public String toString( ) {
		// boolean detail = shouldReport(detailedMessage);
		boolean detail = false;
		String ret = "";
		
		if (edges.size()==0) return "";

		// System.out.println("Checking one equation in env: "+path.env);
		ret += "\n----Start of one path----\n";
		ElementNode leftmost = (ElementNode) getFirst();
//		leftmost.setCause();
		ret += leftmost.getElement().toHTMLString()+"\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
//			ret += "--> (" + (edge.toString()) + ")\n";
//			if (finder.getPath(leftmost, edge.to)!=null)
				ret += ((ElementNode)edge.to).getElement().toHTMLString()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
