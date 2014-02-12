package util;

import graph.ConstraintPath;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;

import diagnostic.UnsatPaths;
import diagnostic.explanation.Entity;

/**
 * that returns sets of EntityType that maximize the term C1*|E|+C2*k_E, where
 * |E| is set size and k_E is # successful paths that use entity in E.
 */

public class EntityExplanationFinder extends HeuristicSearch {
    private HashMap<Entity, Set<ConstraintPath>> dep = new HashMap<Entity, Set<ConstraintPath>>();
    private RankingMetric metric;
    
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates) {
    	this(paths, candidates, 3, 1);
    }
    
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates, double C1, double C2) {
    	super (candidates, paths);
    	for (Entity en : candidates) {
    		dep.put(en, mapsTo(en));
    	}
    	metric = new RankingMetric(C1, C2);
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
		double real = metric.getScore(set.size(),succSum);
		double est = metric.getScore(Estimate(remaining, index),0);
		double key = real + est;
		SearchNode newnode = new SearchNode(set, index, remaining, key);
		queue.offer(newnode);
    }
    
    private Set<ConstraintPath> mapsTo (Entity en) {
    	Set<ConstraintPath> ret = new HashSet<ConstraintPath>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		if (en.explains(path))
   				ret.add(path);
    	}
		return ret;
    }
}
