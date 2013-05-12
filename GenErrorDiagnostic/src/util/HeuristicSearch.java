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
    double C1 = 2;
    double C2 = 1;
    int MAX_SUG = 5;
//    UnsatPaths unsatPaths;
    
    public HeuristicSearch(UnsatPaths paths, Set<EntityType> candidates, Map<String, Double> succCount) {
    	this.paths = paths;
    	this.candidates = (EntityType[])candidates.toArray();
    	this.succCount = succCount;
    	dep = getDependency();
    }

    // estimate the "cost" of satisfying remaining paths
    public int Estimate(Collection<ConstraintPath> paths, int index, List<EntityType> visited) {
    	counter++;
    	
/*    	
  		return 0; // version 1: no guidance at all
 */


	// version 2: little guidance
    	if (paths.size()==0)
    		return 0;
        
        for (int i=index; i<candidates.length; i++) {
			EntityType cand = candidates[i];
			if (visited.contains(cand))
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
    	Set<EntityType> set;
		List<ConstraintPath> remaining;
		int index;
    	
    	public SearchNode(Set<EntityType> set, List<ConstraintPath> remaining, int index) {
    		this.set = set;
    		this.remaining = remaining;
    		this.index = index;
		}
    }
    
    public Set<Set<EntityType>> AStarSearch ( ) {
    	Collection<ConstraintPath> allpaths = paths.getPaths();
    	Set<Set<EntityType>> ret = new HashSet<Set<EntityType>>();
    	FibonacciHeap<SearchNode> heap = new FibonacciHeap<SearchNode>();
    	double N = Harmonic.H(allpaths.size());
    	
    	// explore the first level
    	for (int i=0; i<candidates.length; i++) {
    		EntityType cand = candidates[i];
    		Set<EntityType> set = new HashSet<EntityType>();
			set.add(cand);
			
    		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
    		for (ConstraintPath path : allpaths) {
      			if (!dep.get(cand).contains(path)) {
      				remaining.add(path);
      			}
      		}
    		
    		double real = C1+succCount.get(cand.toString())*C2;
    		double est = C1*Estimate(remaining, i+1, new ArrayList<EntityType>());
    		if (est==-1)
    			continue;
    		FibonacciHeapNode<SearchNode> node = new FibonacciHeapNode<SearchNode>(new SearchNode(set, remaining, i+1), real+est);
    		heap.insert(node, node.getKey());
    	}
    	
    	double best=Double.MAX_VALUE;
    	while (!heap.isEmpty()) {
    		FibonacciHeapNode<SearchNode> minnode = heap.removeMin();
    		SearchNode data = minnode.getData();
    		Set<EntityType> set = data.set;
    		List<ConstraintPath> toSat = data.remaining;
    		// a solution?
    		if (toSat.size()==0) {
    			if (best==Double.MAX_VALUE)
    				best = minnode.key;
    			if (minnode.key<=best || ret.size()<MAX_SUG) {
    				ret.add(set);
    				continue;
    			}
    			else {
    				System.out.println("Est function is called # of times: "+counter);
    				return ret;
    			}
    		}
    		
    		// explore the next level
        	for (int i=data.index; i<candidates.length; i++) {
        		EntityType cand = candidates[i];
        		
        		HashSet<EntityType> toadd = new HashSet<EntityType>(set);
    			toadd.add(cand);
    			
        		List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
        		for (ConstraintPath path : toSat) {
          			if (!dep.get(cand).contains(path)) {
          				remaining.add(path);
          			}
          		}
        		
        		double succSum=0;
        		for (EntityType type : toadd) {
        			succSum+=succCount.get(type.toString());
        		}
        		double real = C1*toadd.size()+succSum*C2;
        		double est = C1*Estimate(remaining, i+1, new ArrayList<EntityType>());
        		if (est==-1)
        			continue;
        		FibonacciHeapNode<SearchNode> newnode = new FibonacciHeapNode<SearchNode>(new SearchNode(toadd, remaining, i+1), real+est);
        		heap.insert(newnode, newnode.getKey());
        	}
    	}
    	
    	return ret;
    }
    
    public abstract Set<ConstraintPath> mapsTo (EntityType element);

}
