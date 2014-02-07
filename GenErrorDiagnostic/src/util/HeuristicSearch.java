package util;

import graph.ConstraintPath;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Set;

import diagnostic.UnsatPaths;
import diagnostic.explanation.Entity;
import diagnostic.explanation.Explanation;

/**
 * This class implements the main functionality of A* search. Given a set of
 * entities, method <code>AStarSearch()</code> returns all subsets of entities
 * so that a metric defined in subclass is minimized.
 */
public abstract class HeuristicSearch {
    protected final Entity[] candidates;				// a set of candidates to be searched
    private double best=Double.MAX_VALUE;		// current best value
    protected int MAX_SUG = 5;					// if specified, return sub-maximum suggestions
    protected UnsatPaths paths;
    
    /**
     * @param candidates a set of candidates to be searched
     */
    public HeuristicSearch(Entity[] candidates, UnsatPaths paths) {
    	this.candidates = candidates;
    	this.paths = paths;
	}   
    
    /**
     * 
     * 
     */
    protected class SearchNode {
    	private Set<Integer> entities;	// a subset of entities (by index)
		private int index;			// the largest searched index to avoid duplication 
		private double est;			// cost estimation
		private List<ConstraintPath> remaining; // remaining paths to be solved
    	
		/**
		 * 
		 * @param entities a subset of entities (represented by their indices)
		 * @param index the largest entity index that is already searched (avoid duplication) 
		 * @param remaining remaining paths to be solved
		 * @param est cost estimation
		 */
    	public SearchNode(Set<Integer> entities, int index, List<ConstraintPath> remaining, double est) {
    		this.entities = entities;
    		this.index = index;
    		this.est = est;
    		this.remaining = remaining;
    	}
    	
    	/**
    	 * @return remaining unsatisfiable paths to cover
    	 */
    	public List<ConstraintPath> getRemaining() {
			return remaining;
		}
    	
    	/**
    	 * @return entity set
    	 */
    	public Set<Integer> getEntities() {
			return entities;
		}
    }
    
    public Set<Explanation> AStarSearch ( ) {
    	Set<Explanation> ret = new HashSet<Explanation>();
    	PriorityQueue<SearchNode> queue = new PriorityQueue<SearchNode>(
    			100, new Comparator<SearchNode>() {
					public int compare(SearchNode n1, SearchNode n2) {
						return (int)(n1.est - n2.est);
					}
				});
    	
    	// explore the first level
    	for (int i=0; i<candidates.length; i++) {
    		addSerchNode(queue, i, null, i+1);
    	}
    	
    	while (!queue.isEmpty()) {
    		SearchNode data = queue.poll();
    		boolean stop = goalTest(ret, data, data.est);
    		if (stop)
    			return ret;
    		
    		// explore the next level
        	for (int i=data.index; i<candidates.length; i++) {
        		addSerchNode(queue, i, data, i+1);
        	}
    	}
    	
    	return ret;
    }
    
    // return true if the search is done
    private boolean goalTest (Set<Explanation> ret, SearchNode node, double key) {
    	if (node.remaining.size()!=0)
    		return false;
    	else {
    		// test if this is an end node before searching deeper
			if (best==Double.MAX_VALUE)
				best = key;
			if (key<=best /*|| ret.size()<MAX_SUG*/) {
				Set<Entity> eset = new HashSet<Entity>();
				for (Integer j:node.entities) {
					eset.add(candidates[j]);
				}
				ret.add( new Explanation(eset, key));
				return false;
			}
			else {
		    	return true;
			}
		}
    }
    
    abstract protected void addSerchNode (PriorityQueue<SearchNode> queue, int candIdx, SearchNode previous, int index);
}
