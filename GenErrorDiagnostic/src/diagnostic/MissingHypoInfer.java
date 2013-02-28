package diagnostic;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import util.AttemptGoal;
import util.MinCutFinder;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;

public class MissingHypoInfer {
	
    public static Set<Set<Hypothesis>> genAssumptions (Set<AttemptGoal> remaining, HashMap<Environment, Environment> cachedEnv) {    	    	
    	HashMap<AttemptGoal, Set<Hypothesis>> dep = genAssumptionDep(remaining, cachedEnv);
    	
    	Set<Hypothesis> candidates = new HashSet<Hypothesis>();
    	for (Set<Hypothesis> s : dep.values())
    		candidates.addAll(s);
 
   		return MinCutFinder.findMinCut(candidates, dep);
   		
    }
    
    // this function returns a hashmap, that gives a set of "weaker" assumptions for each goal
    // Reuse cached env for better performance
    public static HashMap<AttemptGoal, Set<Hypothesis>> genAssumptionDep (Set<AttemptGoal> goals, HashMap<Environment, Environment> cachedEnv) {
    	HashMap<AttemptGoal, Set<Hypothesis>> ret = new HashMap<AttemptGoal, Set<Hypothesis>>( );
    	for (AttemptGoal goal : goals) {
    		Set<Hypothesis> set = new HashSet<Hypothesis>();
    		set.add(goal.getSufficientHypo());
    		ret.put(goal, set);	// the goal itself is a weaker assumption
    		
    		for (AttemptGoal candidate : goals) {
    			if (candidate.equals(goal)) continue;

    			Environment env = goal.getEnv().addLeq(candidate.getSource(), candidate.getSink());
    			
				if (cachedEnv.containsKey(env))
					env = cachedEnv.get(env);
				else {
					cachedEnv.put(env, env);
				}
    			
    			if (env.leq( goal.getSource(), goal.getSink())) {
    				set.add(candidate.getSufficientHypo());
    			}
    		}
    	}
    	return ret;
    }

}
