package constraint.graph;

import java.util.ArrayList;
import java.util.List;

class ConstraintPath {
    List<Edge> edges;
//    LabelEnv env;
//    LinkedHashMap<AccessPath, AccessPath> accesspath = new LinkedHashMap<AccessPath, AccessPath>();
    
    public ConstraintPath(List<Edge> edges) {
        this.edges = edges;
        
        // try to remove cycles
//        Node duplicate = null;
//        List<Node> visited = new ArrayList<Node>();
//        if (edges.size()!=0) {
//        	visited.add(edges.get(0).from);
//        	for (Edge e : edges) {
//        		if (visited.contains(e.to))
//        			duplicate = e.to;
//        		visited.add(e.to);
//        	}
//        	
//        	List<Edge> ret = new ArrayList<Edge>();
//        	if (duplicate!=null) {
//        		int lastindex = 0;
//        		boolean foundFirst = false;
//        		for (int i=0; i<edges.size(); i++) {
//        			Edge e = edges.get(i);
//        			if (!foundFirst && !e.to.equals(duplicate))
//        				ret.add(e);
//        			else if (!foundFirst && e.to.equals(duplicate)) {
//        				ret.add(e);
//        				foundFirst = true;
//        			}
//        			else if (foundFirst && e.to.equals(duplicate)){
//        				lastindex = i;
//        			}
//        		}
//        		for (int j=lastindex+1; j<edges.size(); j++)
//        			ret.add(edges.get(j));
//            	this.edges = ret;
//        	}
//        }
//        PrincipalHierarchy ph = emptyLabelEnv.principalHierarchy();
//        Set<LabelLeAssertion> labelAssertions = new HashSet<LabelLeAssertion>();
//        Set<LabelEnv_c> visited = new HashSet<LabelEnv_c>();
//
//        if (nodes.size()>=2) {
//            for (int i=0; i<nodes.size()-1; i++) {
//               LabelEnv_c temp = (LabelEnv_c) nodes.get(i).outs.get(nodes.get(i+1)).getEnv();
//               if (temp != null && !visited.contains(temp)) {
//                   visited.add(temp);
//                   ph.addHierarchy(temp.principalHierarchy());
//                   labelAssertions.addAll(temp.labelAssertions());
////                   accesspath.putAll(temp.getAccessPathEquivReps());
//               }
//            }
//            String displayLblAssertions = "";
//            for (LabelLeAssertion assertion : labelAssertions) {
//                displayLblAssertions += assertion.toString()+"\n";
//            }
//            env = new LabelEnv_c(ts, ph, new LinkedList<LabelLeAssertion>(labelAssertions), displayLblAssertions, true, true, accesspath, null);
//        }
//        else
//            env = emptyLabelEnv;
    }

    int size () {
        return edges.size();
    }
        
    // increase # path each node appears in    
	public void increaseTotal() {
		for (Edge edge : edges) {
			edge.to.totalcount++;
		}
	}
    
    // a path is valid if all the PathConditions can be reduced to ID
//    public boolean isValid (Graph graph) {
//    	Node current = nodes.get(0);
//    	Node next;
//    	Stack<EdgeCondition> stack = new Stack<EdgeCondition>();
//		for (int i = 1; i < size(); i++) {
//			next = nodes.get(i);
//			Edge edge = graph.getEdge(current, next);
//			if (edge instanceof ConstructorEdge) {
//				EdgeCondition c = ((ConstructorEdge) edge).getCondition();
//				if (!c.isReverse()) {
//					stack.push(c);
//				} else {
//					// try to match a condition on the stack
//					if (stack.empty()) {
////						System.out.println("false");
//						return false;
//					}
//					EdgeCondition top = stack.peek();
//					if (top != null && !top.matches(c))
//						return false;
//					else {
//						stack.pop();
//					}
//				}
//			}
//			current = next;
//		}
//    	if (stack.empty()) {
//    		return true;
//    	}
//    	else
//    		return false;
//    }
    
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
	
	// TODO: use ENV later
    public boolean isSatisfiable () {
    	if (edges.size()<2) 
    		return true;
    	else
    		return (getFirst().equals(getLast()));
    }
    
	public String toString(Graph graph) {
		// boolean detail = shouldReport(detailedMessage);
		boolean detail = false;
		String ret = "";

		// System.out.println("Checking one equation in env: "+path.env);
		ret += "\n----Start of one path----\n";
		ElementNode leftmost = (ElementNode) getFirst();
//		leftmost.setCause();
		ret += leftmost.getName()+"\n";
		for (int k = 0; k < size(); k++) {
			Edge edge = edges.get(k);
			edge.setCause();
			ret += "--> (" + (edge.toString()) + ")\n";
			ret += ((ElementNode)edge.to).getName()+"\n";
		}
		ret += "----End of one path----\n";
		return ret;
	}
}
