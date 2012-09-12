package constraint.graph;

import java.util.List;

import constraint.ast.Environment;

public class ConstraintPath {
    List<Edge> edges;
    Environment assumption;
    
    public ConstraintPath(List<Edge> edges) {
        this.edges = edges;
        this.assumption = new Environment();
        for (Edge edge : edges) {
        	assumption.addEnv(edge.getAssumption());
        }
    }

    int size () {
        return edges.size();
    }
    
    public List<Edge> getEdges() {
		return edges;
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
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
			ret += "--> (" + (edge.toString()) + ")\n";
			ret += ((ElementNode)edge.to).getName()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
