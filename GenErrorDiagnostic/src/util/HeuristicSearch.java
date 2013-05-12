package util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.graph.ConstraintPath;
import diagnostic.UnsatPaths;

// we do an iterative deeping search until at least one cut is returned
public abstract class HeuristicSearch<EntityType> {
	private static int counter =0;
    UnsatPaths paths;
    EntityType[] candidates;
	Map<String, Double> succCount;
	HashMap<EntityType, Set<ConstraintPath>> dep;
    double C1 = 1;
    double C2 = 0;
	double best=Double.MAX_VALUE;
    int MAX_SUG = 5;
//    UnsatPaths unsatPaths;
    
    public HeuristicSearch(UnsatPaths paths, Set<EntityType> candidates, Map<String, Double> succCount) {
    	this.paths = paths;
    	this.candidates = (EntityType[])candidates.toArray();
    	this.succCount = succCount;
    	dep = getDependency();
    	System.out.println(candidates.size());
    }

    // estimate the "cost" of satisfying remaining paths
    public int Estimate(Collection<ConstraintPath> paths, int index) {
    	counter++;
    	
/*    	
  		return 0; // version 1: no guidance at all
 */


	// version 2: little guidance
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
		
//		// there is no way to make a cut
//		if (maxEle==null) {
//			return -1;
//		}
//		// add maxEle to cut
//		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
//		for (ConstraintPath path : paths) {
//  			if (!dep.get(maxEle).contains(path)) {
//  				remaining.add(path);
//  			}
//  		}
//		visited.add(maxEle);
//		return 1+Estimate(remaining, index, visited);
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
    	
    	public SearchNode(Set<Integer> set, List<ConstraintPath> remaining, int index) {
    		this.set = set;
    		this.remaining = remaining;
    		this.index = index;
		}
    }
    
    public Set<Set<EntityType>> AStarSearch ( ) {
    	Collection<ConstraintPath> allpaths = paths.getPaths();
    	Set<Set<EntityType>> ret = new HashSet<Set<EntityType>>();
    	FibonacciHeap<SearchNode> heap = new FibonacciHeap<SearchNode>();
    	
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
    		
    		double real = C1+succCount.get(cand.toString())*C2;
    		double est = C1*Estimate(remaining, i+1);
    		boolean stop = addSerchNode(ret, heap, set, remaining, real+est, i+1);
    		if (stop)
    			return ret;
    	}
    	
    	while (!heap.isEmpty()) {
    		FibonacciHeapNode<SearchNode> minnode = heap.removeMin();
    		SearchNode data = minnode.getData();
    		Set<Integer> set = data.set;
    		List<ConstraintPath> toSat = data.remaining;
    		
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
        		        		
        		double succSum=0;
        		for (Integer j : toadd) {
        			succSum+=succCount.get(candidates[j].toString());
        		}
        		double real = C1*toadd.size()+succSum*C2;
        		double est = C1*Estimate(remaining, i+1);
        		boolean stop = addSerchNode(ret, heap, toadd, remaining, real+est, i+1);
        		if (stop)
        			return ret;
        	}
    	}
    	
    	return ret;
    }
    
    // return true if the search is done
    public boolean addSerchNode (Set<Set<EntityType>> ret, FibonacciHeap<SearchNode> heap, Set<Integer> set, List<ConstraintPath> remaining, double key, int index) {
    	// test if this is a end node before searching deeper
		if (remaining.size()==0) {
			if (best==Double.MAX_VALUE)
				best = key;
			if (key<=best || ret.size()<MAX_SUG) {
				Set<EntityType> eset = new HashSet<EntityType>();
				for (Integer j:set) {
					eset.add(candidates[j]);
				}
				ret.add(eset);
				return false;
			}
			else {
				System.out.println("Est function is called # of times: "+counter);
				return true;
			}
		}
		FibonacciHeapNode<SearchNode> newnode = new FibonacciHeapNode<SearchNode>(new SearchNode(set, remaining, index), key);
		heap.insert(newnode, newnode.getKey());
		return false;
    }
    
    public abstract Set<ConstraintPath> mapsTo (EntityType element);

}
