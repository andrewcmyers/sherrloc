package util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;

import constraint.graph.ConstraintPath;
import constraint.graph.ElementNode;
import constraint.graph.Node;
import diagnostic.Entity;
import diagnostic.UnsatPaths;

/**
 * that returns sets of EntityType that maximize the term C1*|E|+C2*k_E, where
 * |E| is set size and k_E is # successful paths that use entity in E.
 */

public class EntityExplanationFinder extends HeuristicSearch {
	static final double C1 = 3;
    static final double C2 = 1;
    private HashMap<Entity, Set<ConstraintPath>> dep = new HashMap<Entity, Set<ConstraintPath>>();
    
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates) {
    	super (candidates, paths);
    	for (Entity en : candidates) {
    		dep.put(en, mapsTo(en));
    	}
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
			if (dep.get(cand).size()<paths.size())
				continue;
			
			boolean iscut=true;
			for (ConstraintPath p : paths) {
				if (!dep.get(cand).contains(p)) {
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
  			if (!dep.get(cand).contains(path)) {
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
    
    private Set<ConstraintPath> mapsTo (Entity en) {
    	Set<ConstraintPath> ret = new HashSet<ConstraintPath>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		for (Node n : path.getAllNodes()) {
    			if (en.matches((ElementNode)n))
    				ret.add(path);
    		}
    	}
		return ret;
    }
}
