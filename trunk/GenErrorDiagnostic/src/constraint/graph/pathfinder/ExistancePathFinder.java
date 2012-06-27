package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.List;

import constraint.graph.Edge;
import constraint.graph.Graph;
import constraint.graph.IdEdge;
import constraint.graph.LeftEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

/**
 * 
 * Path finder that always returns an empty path if there is an unsatisfiable one.
 *
 */

public class ExistancePathFinder extends CFLPathFinder {
	
	public ExistancePathFinder(Graph g) {
		super(g);
	}
	
	@Override
	public void saturation() {
//		 Suppose the current reduction edge has form B<i,j>, look for
//		 productions with form A := C B
//		 for C<k,i> add edge A<k,j>
		List<Edge> workingList = g.getAllEdges();
		
		for (Edge currentedge : workingList) {
			List<ReductionEdge> toAdd  = new ArrayList<ReductionEdge>();
		
			for (Node from : g.getAllNodes()) {
				if (!currentedge.getTo().equals(from)) {
					Node to = currentedge.getTo();
					for (Edge e : getReductionEdges(from, currentedge.getFrom())) {
						ReductionEdge newedge = null;
	//					System.out.println ("testing "+from+"->"+currentedge.from+"->"+currentedge.to);
						
						// id := id id
						if (e instanceof IdEdge
								&& currentedge instanceof IdEdge) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((IdEdge)e).getEdges());
							list.addAll(((IdEdge)currentedge).getEdges());
							newedge = new IdEdge(from, to, list);
						}
						
						// id := left right
						else if (e instanceof LeftEdge && currentedge instanceof RightEdge 
								&& ((LeftEdge)e).cons.matches(((RightEdge)currentedge).cons)) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeftEdge)e).getEdges());
							list.addAll(((RightEdge)currentedge).getEdges());
							newedge = new IdEdge(from, to, list);
						}
						
						// left := left id
						else if (e instanceof LeftEdge
								&& currentedge instanceof IdEdge) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeftEdge)e).getEdges());
							list.addAll(((IdEdge)currentedge).getEdges());
							newedge = new LeftEdge(
									((LeftEdge) e).cons, from, to, list);
						}
	
						if (newedge != null && !hasReductionEdge(newedge)) {
							toAdd.add(newedge);
						}
					}
				}
			}
			
			
			for (ReductionEdge edge : toAdd) {
				addReductionEdge(edge.getFrom(), edge.getTo(), edge);
				workingList.add(edge);	
			}
		}
	}

	@Override
	protected List<Edge> _getPath(Node start, Node end) {
		if (getIdEdge(start, end)==null)
			return null;
		else
			return new ArrayList<Edge>();
	}

}
