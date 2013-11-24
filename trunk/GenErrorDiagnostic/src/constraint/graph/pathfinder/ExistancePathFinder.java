package constraint.graph.pathfinder;

import java.util.ArrayList;
import java.util.List;

import constraint.graph.ConstraintGraph;
import constraint.graph.Edge;
import constraint.graph.LeftEdge;
import constraint.graph.LeqEdge;
import constraint.graph.Node;
import constraint.graph.ReductionEdge;
import constraint.graph.RightEdge;

/**
 * 
 * Path finder that always returns an empty path if there is an unsatisfiable one.
 *
 */

public class ExistancePathFinder extends CFLPathFinder {
	
	public ExistancePathFinder(ConstraintGraph g) {
		super(g);
	}
	
	@Override
	public void saturation() {
//		 Suppose the current reduction edge has form B<i,j>, look for
//		 productions with form A := C B
//		 for C<k,i> add edge A<k,j>
		List<ReductionEdge> workingList = getAllReductionEdges();
		
		while ( !workingList.isEmpty()) {
			Edge currentedge = workingList.remove(0);
			
			List<ReductionEdge> toAdd  = new ArrayList<ReductionEdge>();
		
			for (Node from : g.getAllNodes()) {
				if (!currentedge.getTo().equals(from)) {
					Node to = currentedge.getTo();
					for (Edge e : getReductionEdges(from, currentedge.getFrom())) {
						ReductionEdge newedge = null;
	//					System.out.println ("testing "+from+"->"+currentedge.from+"->"+currentedge.to);
						
						// id := id id
						if (e instanceof LeqEdge
								&& currentedge instanceof LeqEdge) {
							newedge = new LeqEdge(from, to, e, currentedge);
						}
						
						// id := left right
						else if (e instanceof LeftEdge && currentedge instanceof RightEdge 
								&& ((LeftEdge)e).cons.matches(((RightEdge)currentedge).cons)) {
							newedge = new LeqEdge(from, to, e, currentedge);
						}
						
						// left := left id
						else if (e instanceof LeftEdge
								&& currentedge instanceof LeqEdge) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeftEdge)e).getEdges());
							list.addAll(((LeqEdge)currentedge).getEdges());
							newedge = new LeftEdge(
									((LeftEdge) e).cons, from, to, e, currentedge);
						}
	
						if (newedge != null && !hasReductionEdge(newedge)) {
							toAdd.add(newedge);
						}
					}
				}
			}
			
			// handle the case when current edge is on the left
			for (Node to : g.getAllNodes()) {
				if (!currentedge.getFrom().equals(to)) {
					Node from = currentedge.getFrom();
					for (Edge e : getReductionEdges(currentedge.getTo(), to)) {
						ReductionEdge newedge = null;
	//					System.out.println ("testing "+from+"->"+currentedge.from+"->"+currentedge.to);
						
						// id := id id
						if (e instanceof LeqEdge
								&& currentedge instanceof LeqEdge) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeqEdge)currentedge).getEdges());
							list.addAll(((LeqEdge)e).getEdges());
							newedge = new LeqEdge(from, to, currentedge, e);
						}
						
						// id := left right
						else if (e instanceof RightEdge && currentedge instanceof LeftEdge 
								&& ((LeftEdge)currentedge).cons.matches(((RightEdge)e).cons)) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeftEdge)currentedge).getEdges());
							list.addAll(((RightEdge)e).getEdges());
							newedge = new LeqEdge(from, to, currentedge, e);
						}
						
						// left := left id
						else if (currentedge instanceof LeftEdge
								&& e instanceof LeqEdge) {
							List<Edge> list = new ArrayList<Edge>();
							list.addAll(((LeftEdge)currentedge).getEdges());
							list.addAll(((LeqEdge)e).getEdges());
							newedge = new LeftEdge(
									((LeftEdge) currentedge).cons, from, to, currentedge, e);
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
		if (getLeqEdge(start, end)==null)
			return null;
		else
			return getLeqEdge(start,end).getEdges();
	}
	
	@Override
	protected List<ReductionEdge> _getLeftPath(Node start, Node end) {
		return null; // do nothing
	}
}
