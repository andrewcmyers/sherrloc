package diagnostic;

import graph.ConstraintPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.HeuristicSearch;
import util.MinCutFinder;
import diagnostic.explanation.Entity;
import diagnostic.explanation.Explanation;
import diagnostic.explanation.HypothesisEntity;

/**
 * Infers "minimum weakest hypotheses" that best explain unsatisfiable paths <code>paths</code>
 */
public class MissingHypoInfer extends InferenceEngine {

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
	
	public Set<Explanation> infer() {
		Set<Entity> cand = getCandidates();
		return getAlogithm(cand).AStarSearch();
	}
    
	// number of missing hypotheses, used for unit test
    public int getAssumptionNumber () {
        Set<Explanation> result = infer();
    	return result.size();
    }
    
	public String getAssumptionString() {
		Set<Explanation> result = infer();
		StringBuffer sb = new StringBuffer();
		for (Explanation s : result) {
			List<String> list = new ArrayList<String>();
			for (Entity en : s.getEntities())
				list.add(en.toString());
			Collections.sort(list);
			for (String str : list)
				sb.append(str + ";");
			sb.append("\n");
		}
		return sb.toString();
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
