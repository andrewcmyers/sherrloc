package diagnostic;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import util.MinCutFinder;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.graph.ConstraintPath;

public class MissingHypoInfer {
	
	
    public static Set<Set<Hypothesis>> genAssumptions (UnsatPaths paths, Map<Environment, Environment> env) {    	    	
    	final Set<Hypothesis> candidates = new HashSet<Hypothesis>();
    	final Map<Environment, Environment> cachedEnv = env;
    	for (ConstraintPath path : paths.getPaths())
    		candidates.add(path.getMinHypo());
    	    	
    	MinCutFinder<Hypothesis> cutFinder = new MinCutFinder<Hypothesis>(paths) {
			@Override
			public Set<Hypothesis> mapsTo(ConstraintPath path) {
				return genAssumptionDep(path, candidates, cachedEnv);
			}
		};
 
   		return cutFinder.findMinCut();
   		
    }
    
    // this function returns a hashmap, that gives a set of "weaker" assumptions for each goal
    // Reuse cached env for better performance
    private static Set<Hypothesis> genAssumptionDep (ConstraintPath path, Set<Hypothesis> candidates, Map<Environment, Environment> cachedEnv) {
		Set<Hypothesis> ret = new HashSet<Hypothesis>();
		Hypothesis minHypo = path.getMinHypo();
		ret.add(minHypo);

		for (Hypothesis candidate : candidates) {
			if (candidate.equals(path.getMinHypo()))
				continue;

			Environment env = path.getAssumption().addLeq(candidate.getFirst(),
					candidate.getSecond());

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
