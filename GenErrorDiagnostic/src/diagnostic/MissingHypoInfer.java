package diagnostic;

import graph.ConstraintPath;

import java.util.HashSet;
import java.util.Set;

import util.HeuristicSearch;
import util.MinCutFinder;
import diagnostic.explanation.Entity;
import diagnostic.explanation.HypothesisEntity;

/**
 * Infers "minimum weakest hypotheses" that best explain unsatisfiable paths
 * <code>paths</code>. Refer to section 5.3 in paper
 * "Toward General Diagnosis of Static Errors" by Danfeng Zhang and Andrew C.
 * Myers for the formal definition and algorithm
 */
public class MissingHypoInfer extends InferenceEngine {

	/**
	 * @param paths
	 *            Unsatisfiable paths identified by constraint analysis
	 */
	public MissingHypoInfer(UnsatPaths paths) {
		super(paths);
	}
	
	@Override
	public Set<Entity> getCandidates() {
		Set<Entity> cand = new HashSet<Entity>();
    	for (ConstraintPath path : paths.getPaths())
    		cand.add(new HypothesisEntity(path.getMinHypo()));
    	return cand;
	}
	
	@Override
	public HeuristicSearch getAlogithm(Set<Entity> candidates) {
		return new MinCutFinder(paths, candidates.toArray(new Entity[candidates.size()]));
	}
    
    @Override
    public String HTMLinfo() {
    	return "<H3>Likely missing assumption(s): </H3>\n";
    }
    
    @Override
    public String info() {
    	return "Likely missing assumption(s): \n";
    }
}
