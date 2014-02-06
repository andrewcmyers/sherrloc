package diagnostic;

import graph.ConstraintPath;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import util.MinCutFinder;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;

public class MissingHypoInfer {
	
	
    public static Set<Explanation> genAssumptions (UnsatPaths paths, Map<Environment, Environment> env) {    	    	
    	final Set<HypothesisEntity> cand = new HashSet<HypothesisEntity>();
    	for (ConstraintPath path : paths.getPaths())
    		cand.add(new HypothesisEntity(path.getMinHypo(), env));
    	    	
    	MinCutFinder cutFinder = new MinCutFinder(paths, cand.toArray(new Entity[cand.size()]));
 
   		return cutFinder.AStarSearch();
   		
    }
    
    // this function returns a hashmap, that gives a set of "weaker" assumptions for each path
    // Reuse cached env for better performance
    public static Set<Hypothesis> genAssumptionDep (ConstraintPath path, Set<Hypothesis> candidates, Map<Environment, Environment> cachedEnv) {
		Set<Hypothesis> ret = new HashSet<Hypothesis>();
		Hypothesis minHypo = path.getMinHypo();
		ret.add(minHypo);

		for (Hypothesis candidate : candidates) {
			if (candidate.equals(path.getMinHypo()))
				continue;

			Environment env = path.getAssumption().addLeq(candidate.getFirst(),candidate.getSecond());
			
			if (cachedEnv.containsKey(env))
				env = cachedEnv.get(env);
			else {
				cachedEnv.put(env, env);
			}
			
			if (env.leq(minHypo.getFirst(), minHypo.getSecond())) {
				ret.add(candidate);
			}
		}
    	return ret;
    }

}
