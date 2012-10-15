package constraint.graph;

import java.util.ArrayList;
import java.util.List;

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
    
	public List<Node> getNodes( ) {
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
        
    // increase # path each node appears in    
	public void increaseTotal() {
		for (Edge edge : edges) {
			edge.to.totalcount++;
		}
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

		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.incSuccCounter();
		}
	}
	
	public void setCause ( ) {
		if (edges.size()==0) return;

		ElementNode leftmost = (ElementNode) getFirst();
//		leftmost.setCause();
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
		}
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
		ret += leftmost.getName()+"\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret += "--> (" + (edge.toString()) + ")\n";
			ret += ((ElementNode)edge.to).getName()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
