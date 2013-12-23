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
        if (edges.size()==0) {
        	this.minHypo = null;
        }
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
		Stack<Node> leqNodes = new Stack<Node>();
		Stack<EdgeCondition> conditions = new Stack<EdgeCondition>();
		int length = 0;
		Node first = getFirst();
		leqNodes.push(first);
		
		for (int k = 0; k < edges.size(); k++) {
			Edge edge = edges.get(k);
			Node eto = edge.to;
			boolean needCmp = eto.getElement() instanceof Constructor || eto.getElement() instanceof ComplexElement
					|| eto.getElement() instanceof JoinElement || eto.getElement() instanceof MeetElement;
			if (edge instanceof EquationEdge || edge instanceof JoinEdge ||
					edge instanceof MeetEdge) {
				if (needCmp) {
					if (conditions.size()==0) {
						if (length>0)
							return false;
						else
							length ++;
					}
					
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
					if (needCmp) {
						if (conditions.size()==0) {
							if (length>0)
								return false;
							else
								length ++;
						}
						
						if (!leqNodes.empty() && !env.leq(leqNodes.peek().getElement(), eto.getElement()))
							return false;
						else {
							leqNodes.pop();
							leqNodes.push(eto);
						}
					}
				}
			}
		}

		return true;
	}
	
	public boolean isUnsatPath(HashMap<Environment, Environment> cachedEnv) {
		if (edges.size()==0) return false;
		Environment env = getEnv_(cachedEnv);
		
		return !env.leq(getFirst().getElement(), getLast().getElement());
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
		return getFirst().getElement();
	}
	
	public Element getLastElement () {
		return getLast().getElement();
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

		Node leftmost = getFirst();
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
		
	public void setCause ( ) {
		if (edges.size()==0) return;

		Node leftmost = getFirst();
		leftmost.setCause();
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.from.setCause();
			edge.setCause();
			edge.to.setCause();
		}
	}
	    
	public String toString( ) {
		String ret = "";
		
		if (edges.size()==0) return "";

		// System.out.println("Checking one equation in env: "+path.env);
		ret += "\n----Start of one path----\n";
		Node leftmost = getFirst();
//		leftmost.setCause();
		ret += leftmost.getElement().toString()+"\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			ret += "--> (" + (edge.toString()) + ")\n";
//			if (finder.getPath(leftmost, edge.to)!=null)
				ret += edge.to.getElement().toString()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
