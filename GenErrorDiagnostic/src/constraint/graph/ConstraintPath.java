package constraint.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import constraint.ast.ComplexElement;
import constraint.ast.Constructor;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.graph.pathfinder.PathFinder;

public class ConstraintPath {
    List<Edge> edges;
    Environment assumption;
    PathFinder finder;
    Hypothesis minHypo;
    
    public ConstraintPath(List<Edge> edges, PathFinder finder, Environment globalEnv) {
        this.edges = edges;
        assumption = new Environment();
        assumption.addEnv(globalEnv);
        for (Edge edge : edges) {
        	assumption.addEnv(edge.getAssumption());
        }
        this.finder =  finder;
        if (edges.size()==0)
        	this.minHypo = null;
        else
        	this.minHypo = new Hypothesis(getFirstElement(), getLastElement());
    }

    int size () {
        return edges.size();
    }
    
    public List<Edge> getEdges() {
		return edges;
	}
    
    public Hypothesis getMinHypo() {
		return minHypo;
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
	
	// a succ path is one where all LEQ are provable and all LEQ nodes are variable free
	public boolean isSuccPath(HashMap<Environment, Environment> cachedEnv) {
		if (edges.size()==0) return false;
		
		// at least one of the edges should be equation edge
		boolean hasEqu = false;
		int varfree = 0;
		for (Edge e : edges) {
			if (e instanceof EquationEdge) {
				hasEqu = true;
				break;
			}
		}
		if (!hasEqu)
			return false;
		
		Environment env = getEnv_(cachedEnv);
		// System.out.println("Checking one equation in env: "+path.env);
		Stack<ElementNode> leqNodes = new Stack<ElementNode>();
		Stack<EdgeCondition> conditions = new Stack<EdgeCondition>();
		ElementNode first = (ElementNode) getFirst();
		leqNodes.push(first);
		
		for (int k = 0; k < edges.size(); k++) {
			Edge edge = edges.get(k);
			ElementNode eto = (ElementNode)edge.to;
			boolean needCmp = eto.getElement() instanceof Constructor || eto.getElement() instanceof ComplexElement
					|| eto.getElement() instanceof JoinElement || eto.getElement() instanceof MeetElement;
			if (edge instanceof EquationEdge) {
				if (needCmp) {
					if (!env.leq(leqNodes.peek().getElement(), eto.getElement()))
						return false;
					else {
						leqNodes.pop();
						leqNodes.push(eto);
					}
				}
			}
			else if (edge instanceof ConstructorEdge) {
				ConstructorEdge cedge = (ConstructorEdge)edge;
				if (conditions.empty() || !conditions.peek().matches(cedge.condition)) {
					conditions.push(cedge.condition);
					leqNodes.push(eto);
				}
				else {
					conditions.pop();
					leqNodes.pop();
				}
				if (needCmp) {
					if (!leqNodes.empty() && !env.leq(leqNodes.peek().getElement(), eto.getElement()))
						return false;
					else {
						leqNodes.pop();
						leqNodes.push(eto);
					}
				}
			}
		}

		return true;
	}
	
	public boolean isUnsatPath(HashMap<Environment, Environment> cachedEnv) {
		if (edges.size()==0) return false;
		Environment env = getEnv_(cachedEnv);
		
		return !env.leq(((ElementNode)getFirst()).getElement(), ((ElementNode)getLast()).getElement());
	}
	
	private Environment getEnv_ (HashMap<Environment, Environment> cachedEnv) {
		if (cachedEnv.containsKey(assumption))
			return cachedEnv.get(assumption);
		else {
			cachedEnv.put(assumption, assumption);
			return assumption;
		}
	}
	
	public Set<Node> getAllNodes( ) {
		HashSet<Node> ret = new HashSet<Node>();
		
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
	
	public Element getFirstElement () {
		return ((ElementNode)getFirst()).getElement();
	}
	
	public Element getLastElement () {
		return ((ElementNode)getLast()).getElement();
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
	
	public void setAssumption(Environment assumption) {
		this.assumption = assumption;
	}
		
	public void incSuccCounter ( ) {
		if (edges.size()==0) return;
		
		// avoid duplicate expressions
		Set<String> processedNodes = new HashSet<String>();
		Set<String> processedEdges = new HashSet<String>();

		ElementNode leftmost = (ElementNode) getFirst();
		leftmost.incSuccCounter();
		processedNodes.add(leftmost.toString());
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			if (!processedEdges.contains(edge)) {
				edge.incSuccCounter();
				processedEdges.add(edge.toString());
			}
			if (!processedNodes.contains(edge.getTo().toString())) {
				edge.getTo().incSuccCounter();
				processedNodes.add(edge.getTo().toString());
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
	    Set<Node> nodes1 = getAllNodes();
	    Set<Node> nodes2 = new HashSet<Node>(path.getAllNodes());
	    for (Node n1 : nodes1) {
	    	if (nodes2.contains(n1))
	    		return true;
//	    	for (Node n2 : nodes2) {
//    			if (((ElementNode)n1).isInCons() && ((ElementNode)n2).isInCons()) {
//    				if (n1.toString().equals(n2.toString())) {
//    					return true;
//    				}
//    			}
//	    	}
	    }
	    return false;
	}
    
	public String toString( ) {
		String ret = "";
		
		if (edges.size()==0) return "";

		// System.out.println("Checking one equation in env: "+path.env);
		ret += "\n----Start of one path----\n";
		ElementNode leftmost = (ElementNode) getFirst();
//		leftmost.setCause();
		ret += leftmost.getElement().toString()+"\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret += "--> (" + (edge.toString()) + ")\n";
//			if (finder.getPath(leftmost, edge.to)!=null)
				ret += ((ElementNode)edge.to).getElement().toString()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
