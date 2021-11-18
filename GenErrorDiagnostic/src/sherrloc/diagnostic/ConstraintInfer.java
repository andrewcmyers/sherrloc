package sherrloc.diagnostic;

import java.util.*;

import sherrloc.constraint.ast.Constraint;
import sherrloc.diagnostic.explanation.ConstraintEntity;
import sherrloc.diagnostic.explanation.Entity;
import sherrloc.diagnostic.explanation.Explanation;
import sherrloc.graph.ConstraintEdge;
import sherrloc.graph.ConstraintPath;
import sherrloc.graph.Edge;

/**
 * Infer the most likely wrong constraint in a program
 */
public class ConstraintInfer extends InferenceEngine {
	private Map<String, Integer> succCount;
	
	/**
	 * @param paths
	 *            All unsatisfiable paths identified in constraint analysis
	 */
	public ConstraintInfer(UnsatPaths paths, List<Edge> allEdges, DiagnosticOptions opt) {
		super(paths, opt);	
		
		// gather # satisfiable paths using an expression (represented by string)
		succCount = new HashMap<String, Integer>();
		for (Edge e : allEdges) {
			if (e instanceof ConstraintEdge) {
				succCount.put(((ConstraintEdge) e).getConstraint().getPos().toString(), 0);
			}
		}
		for (Edge e : allEdges) {
			if (e instanceof ConstraintEdge) {
				Constraint cons = ((ConstraintEdge) e).getConstraint();
				int count = succCount.get(cons.getPos().toString());
				succCount.put(cons.getPos().toString(), count + cons.getNumSuccPaths());
			}
		}
	}
	
	@Override
	public Set<Entity> getCandidates() {
    	Set<Entity> cand = new HashSet<Entity>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		for (Edge edge : path.getEdges()) {
    			if (edge instanceof ConstraintEdge) {
    				Constraint cons = ((ConstraintEdge) edge).getConstraint();
    				String str = cons.getPos().toString();
    				cand.add(new ConstraintEntity(cons, str, cons.getPos().toStringWithExp(), cons.toHTMLString(), cons.toConsoleString(), succCount.get(str), cons.getPos().getWeight()));
    			}
    		}
    	}
    	
    	return cand;
	}

	@Override
	public HeuristicSearch getAlogithm(Set<Entity> candidates) {
    	Entity[] candarr = candidates.toArray(new Entity[candidates.size()]);
    	return new EntityExplanationFinder(paths, candarr, options.getNSubopt());		
	}
	
	@Override
	public String HTMLinfo() {
		return "<H4>Constraints in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n";
	}	
	
	@Override
	public String info() {
		return "Constraints in the source code that appear most likely to be wrong: \n";
	}

	public ArrayList<Explanation> inferResult() {
		ArrayList<Explanation> ret = new ArrayList<>();

		final Set<Entity> cand = getCandidates();
		HeuristicSearch algorithm = getAlogithm(cand);

		Set<Explanation> results = algorithm.findOptimal();

		ArrayList<Explanation> list = new ArrayList<Explanation>();
		for (Explanation set : results) {
			list.add(set);
		}
		Collections.sort(list);
		return list;
	}
}
