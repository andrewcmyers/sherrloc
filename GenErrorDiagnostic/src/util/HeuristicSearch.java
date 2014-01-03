package util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;

import constraint.graph.ConstraintPath;
import diagnostic.UnsatPaths;

// we do a heuristic search that returns an explanation with maximum likelihood
public abstract class HeuristicSearch<EntityType> {
	private int nodes =0;
    UnsatPaths paths;
    EntityType[] candidates;
	Map<String, Double> succCount;
	HashMap<EntityType, Set<ConstraintPath>> dep;
    static final double C1 = 3;
    static final double C2 = 1;
	double best=Double.MAX_VALUE;
    int MAX_SUG = 5;

    public HeuristicSearch(UnsatPaths paths, Set<EntityType> candidates, Map<String, Double> succCount) {
    	this.paths = paths;
    	this.candidates = (EntityType[])candidates.toArray();
    	this.succCount = succCount;
    	dep = getDependency();
    }

    // a heuristic that estimates the "cost" of satisfying remaining paths
    public int Estimate(Collection<ConstraintPath> paths, int index) {
		if (paths.size()==0)
    		return 0;
        
        for (int i=index; i<candidates.length; i++) {
			EntityType cand = candidates[i];
			
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
    
    public HashMap<EntityType, Set<ConstraintPath>> getDependency ( ) {
    	HashMap<EntityType, Set<ConstraintPath>> map = new HashMap<EntityType, Set<ConstraintPath>>();
    	
    	for (EntityType cand : candidates) {
    		map.put(cand, mapsTo(cand));
    	}
    	return map;
    }
    
    protected class SearchNode {
    	Set<Integer> set;
		List<ConstraintPath> remaining;
		int index;
		double est;
    	
    	public SearchNode(Set<Integer> set, List<ConstraintPath> remaining, int index, double est) {
    		this.set = set;
    		this.remaining = remaining;
    		this.index = index;
    		this.est = est;
		}
    }
    
    public Set<Set<EntityType>> AStarSearch ( ) {
    	Collection<ConstraintPath> allpaths = paths.getPaths();
    	Set<Set<EntityType>> ret = new HashSet<Set<EntityType>>();
    	PriorityQueue<SearchNode> queue = new PriorityQueue<SearchNode>(
    			100, new Comparator<SearchNode>() {
					public int compare(SearchNode n1, SearchNode n2) {
						return (int)(n1.est - n2.est);
					}
				});
    	
    	// explore the first level
    	for (int i=0; i<candidates.length; i++) {
    		EntityType cand = candidates[i];
    		Set<Integer> set = new HashSet<Integer>();
			set.add(i);
			
    		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
    		for (ConstraintPath path : allpaths) {
      			if (!dep.get(cand).contains(path)) {
      				remaining.add(path);
      			}
      		}
    		
    		addSerchNode(queue, set, remaining, i+1);
    	}
    	
    	while (!queue.isEmpty()) {
    		SearchNode data = queue.poll();
    		Set<Integer> set = data.set;
    		List<ConstraintPath> toSat = data.remaining;
    		boolean stop = goalTest(ret, data, data.est);
    		if (stop)
    			return ret;
    		
    		// explore the next level
        	for (int i=data.index; i<candidates.length; i++) {
        		EntityType cand = candidates[i];
        		
        		HashSet<Integer> toadd = new HashSet<Integer>(set);
    			toadd.add(i);
    			
        		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
        		for (ConstraintPath path : toSat) {
          			if (!dep.get(cand).contains(path)) {
          				remaining.add(path);
          			}
          		}
        		        		
        		addSerchNode(queue, toadd, remaining, i+1);
        	}
    	}
    	
    	return ret;
    }
    
    // return true if the search is done
    public boolean goalTest (Set<Set<EntityType>> ret, SearchNode node, double key) {
    	if (node.remaining.size()!=0)
    		return false;
    	else {
    		// test if this is a end node before searching deeper
			if (best==Double.MAX_VALUE)
				best = key;
			if (key<=best /*|| ret.size()<MAX_SUG*/) {
				Set<EntityType> eset = new HashSet<EntityType>();
				for (Integer j:node.set) {
					eset.add(candidates[j]);
				}
				ret.add(eset);
				return false;
			}
			else {
		    	return true;
			}
		}
    }
    
    public void addSerchNode (PriorityQueue<SearchNode> queue, Set<Integer> set, List<ConstraintPath> remaining, int index) {
    	double succSum=0;
		for (Integer j : set) {
			succSum+=succCount.get(candidates[j].toString());
		}
		double real = HeuristicSearch.getScore(set.size(),succSum);
		double est = C1*Estimate(remaining, index);
		double key = real + est;
		
		SearchNode newnode = new SearchNode(set, remaining, index, key);
		nodes ++;
		queue.offer(newnode);
    }
    
    public abstract Set<ConstraintPath> mapsTo (EntityType element);
    
    public static double getScore(int setsize, double succ) {
    	return C1*setsize + C2*succ;
    }

}
