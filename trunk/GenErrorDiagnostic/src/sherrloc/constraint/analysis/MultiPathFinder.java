package sherrloc.constraint.analysis;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.Edge;
import sherrloc.graph.EdgeCondition;
import sherrloc.graph.Node;

public class MultiPathFinder extends ShortestPathFinder {
	/**
	 * @param graph
	 *            A graph to be saturated
	 */
	public MultiPathFinder(ConstraintGraph graph, boolean verbose, boolean isHypo) {
		super(graph, verbose, isHypo);
	}
	
	@Override
	protected void addNextHop(Node start, Node end, EdgeCondition type,
			List<Triple> evidence) {
		nextHop[start.getIndex()][end.getIndex()].get(type).add(evidence);
		
//		if (nextHop[start.getIndex()][end.getIndex()].get(type).size()>1) {
//			Set<List<Edge>> eset = new HashSet<List<Edge>>();
//			getLeqPath(start, end, type, eset, false);
//			for (List<Edge> lst : eset) {
//				for (Edge e : lst)
//					System.out.print(e.getFrom() + " --> " + e.getTo());
//				System.out.println();
//			}
//		}
	}
	
}
