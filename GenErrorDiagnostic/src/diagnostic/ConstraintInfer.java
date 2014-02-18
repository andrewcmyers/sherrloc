package diagnostic;

import graph.ConstraintPath;
import graph.Edge;
import graph.EquationEdge;

import java.util.HashSet;
import java.util.Set;

import util.EntityExplanationFinder;
import util.HeuristicSearch;
import constraint.ast.Constraint;
import diagnostic.explanation.ConstraintEntity;
import diagnostic.explanation.Entity;

/**
 * Infer the most likely wrong constraint in a program
 */
public class ConstraintInfer extends InferenceEngine {
	
	/**
	 * @param paths
	 *            All unsatisfiable paths identified in constraint analysis
	 */
	public ConstraintInfer(UnsatPaths paths) {
		super(paths);
	}
	
	@Override
	public Set<Entity> getCandidates() {
    	Set<Entity> cand = new HashSet<Entity>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		for (Edge edge : path.getEdges()) {
    			if (edge instanceof EquationEdge) {
    				Constraint cons = ((EquationEdge) edge).getEquation();
    				cand.add(new ConstraintEntity(cons, cons.getNumSuccPaths()));
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
