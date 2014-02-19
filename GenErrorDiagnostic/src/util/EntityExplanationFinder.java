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
 * Searches for sets of Entity that maximize the term C1*|E|+C2*k_E, where |E|
 * is set size and k_E is # successful paths that use entity in E
 */
public class EntityExplanationFinder extends HeuristicSearch {
    private HashMap<Entity, Set<ConstraintPath>> dep = new HashMap<Entity, Set<ConstraintPath>>();
    private RankingMetric metric;
    
	/**
	 * @see #EntityExplanationFinder(UnsatPaths, Entity[], double, double). Use
	 *      default value C1=3 and C2=1
	 */
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates) {
    	this(paths, candidates, 3, 1);
    }
    
    /**
	 * @param paths
	 *            Unsatisfiable paths identified in constraint analysis
	 * @param candidates
	 *            A set of entities so that all subsets can be a potential
	 *            explanation
	 * @param C1
	 *            Value of C1 in ranking metric C1*|E|+C2*k_E
	 * @param C2
	 *            Value of C2 in ranking metric C1*|E|+C2*k_E
	 */
    public EntityExplanationFinder(UnsatPaths paths, Entity[] candidates, double C1, double C2) {
    	super (candidates, paths);
    	for (Entity en : candidates) {
    		dep.put(en, mapsTo(en));
    	}
    	metric = new RankingMetric(C1, C2);
    }
    	
	/**
	 * A heuristic that estimates the "cost" of satisfying remaining paths. The
	 * estimation is "consistent", in a sense that it never overestimate the
	 * actual cost. This property ensures that the A* search is optimal: all and
	 * only the most likely wrong subsets of entities are returned. See this <a
	 * href="http://hdl.handle.net/1813/33742">technical report</a> for more
	 * details
	 * 
	 * @param paths
	 *            Remaining unsatisfiable paths
	 * @param index
	 *            The index of the next entity to search
	 * @return An estimation of the "cost" of satisfying remaining paths
	 */
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
        
    /**
	 * Add a new node to the frontier of search tree (<code>queue</code>).
	 * 
	 * @param queue
	 *            The frontier of search tree where the new node is added to
	 * @param candIdx
	 *            Index of the entity to be added to the previous search node
	 *            <code>previous</code>
	 * @param previous
	 *            The previous search node
	 */
	public void addSerchNode(PriorityQueue<SearchNode> queue, int candIdx,
			SearchNode previous) {
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
    	
    	// calculate the remaining paths to satisfy after the new entity is added
		for (ConstraintPath path : toSat) {
  			if (!dep.get(cand).contains(path)) {
  				remaining.add(path);
  			}
  		}
    	
		// calculate the cost of the new node
    	double succSum=0;
		for (Integer j : set) {
			succSum+=candidates[j].getSuccCount();
		}
		double real = metric.getScore(set.size(),succSum);
		double est = metric.getScore(Estimate(remaining, candIdx+1),0);
		double key = real + est;
		SearchNode newnode = new SearchNode(set, candIdx, remaining, key);
		queue.offer(newnode);
    }
    
	/**
	 * A lookup table to quickly find constraint paths that the entity
	 * <code>en</code> explains
	 * 
	 * @param en
	 *            An entity
	 * @return Constraint paths that the entity <code>en</code> explains
	 */
    private Set<ConstraintPath> mapsTo (Entity en) {
    	Set<ConstraintPath> ret = new HashSet<ConstraintPath>();
		
    	for (ConstraintPath path : paths.getPaths()) {
    		if (en.explains(path))
   				ret.add(path);
    	}
		return ret;
    }
}
