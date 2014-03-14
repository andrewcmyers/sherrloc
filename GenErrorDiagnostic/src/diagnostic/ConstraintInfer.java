package diagnostic;

import graph.ConstraintEdge;
import graph.ConstraintPath;
import graph.Edge;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.ast.Constraint;
import diagnostic.explanation.ConstraintEntity;
import diagnostic.explanation.Entity;

/**
 * Infer the most likely wrong constraint in a program
 */
public class ConstraintInfer extends InferenceEngine {
	private Map<String, Integer> succCount;
	
	/**
	 * @param paths
	 *            All unsatisfiable paths identified in constraint analysis
	 */
	public ConstraintInfer(UnsatPaths paths, List<Edge> allEdges) {
		super(paths);
		
        // gather # satisfiable paths using an constraint (represented by string)
		succCount = new HashMap<String, Integer>();
		Set<Constraint> constraints = new HashSet<Constraint>();
		
        for (Edge e : allEdges) {
        	if (e instanceof ConstraintEdge) {
        		ConstraintEdge ce = (ConstraintEdge)e;
        		constraints.add(ce.getConstraint());
        		succCount.put(ce.getConstraint().toString(), 0);
        	}
        }
        for (Constraint c : constraints) {
			int count = succCount.get(c.toString());
			succCount.put(c.toString(), count+c.getNumSuccPaths());
        }
	}
	
	@Override
	public Set<Entity> getCandidates() {
    	Set<Entity> cand = new HashSet<Entity>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		for (Edge edge : path.getEdges()) {
    			if (edge instanceof ConstraintEdge) {
    				Constraint cons = ((ConstraintEdge) edge).getConstraint();
    				cand.add(new ConstraintEntity(cons, succCount.get(cons.toString())));
    			}
    		}
    	}
    	
    	return cand;
	}

	@Override
	public HeuristicSearch getAlogithm(Set<Entity> candidates) {
    	Entity[] candarr = candidates.toArray(new Entity[candidates.size()]);
    	return new EntityExplanationFinder(paths, candarr);		
	}
	
	@Override
	public String HTMLinfo() {
		return "<H4>Constraints in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n";
	}	
	
	@Override
	public String info() {
		return "Constraints in the source code that appear most likely to be wrong: \n";
	}	
}
