package util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import constraint.graph.ConstraintPath;
import diagnostic.Entity;
import diagnostic.UnsatPaths;

/**
 * that returns sets of EntityType that maximize the term C1*|E|+C2*k_E, where
 * |E| is set size and k_E is # successful paths that use entity in E.
 */

public class EntityExplanationFinder extends HeuristicSearch {
	static final double C1 = 3;
    static final double C2 = 1;
    
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates) {
    	super (candidates, paths);
    }
    
	static public double getScore(int setsize, double succ) {
    	return C1*setsize + C2*succ;
    }
	
    // a heuristic that estimates the "cost" of satisfying remaining paths
    public int Estimate(Collection<ConstraintPath> paths, int index) {
        
		if (paths.size()==0)
    		return 0;
        
        for (int i=index; i<candidates.length; i++) {
			Entity cand = candidates[i];
			
			// a quick test
			if (mapsTo(cand).size()<paths.size())
				continue;
			
			boolean iscut=true;
			for (ConstraintPath p : paths) {
				if (!mapsTo(cand).contains(p)) {
					iscut=false;
					break;
				}	
			}
			if (iscut) {
				return 1;
			}
		}
		return 2;
    }
        
    public void addSerchNode (PriorityQueue<SearchNode> queue, int candIdx, SearchNode previous, int index) {
		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
    	Collection<ConstraintPath> toSat;
		Set<Integer> set;
		Entity cand = candidates[candIdx];
    	if (previous == null) {
    		toSat = paths.getPaths();
    		set = new HashSet<Integer>();
    	}
    	else {
    		toSat = previous.getRemaining();
    		set = new HashSet<Integer>(previous.getEntities());
    	}
    	set.add(candIdx);
    	
		for (ConstraintPath path : toSat) {
  			if (!mapsTo(cand).contains(path)) {
  				remaining.add(path);
  			}
  		}
    	
    	double succSum=0;
		for (Integer j : set) {
			succSum+=candidates[j].getSuccCount();
		}
		double real = getScore(set.size(),succSum);
		double est = C1*Estimate(remaining, index);
		double key = real + est;
		SearchNode newnode = new SearchNode(set, index, remaining, key);
		queue.offer(newnode);
    }
}
