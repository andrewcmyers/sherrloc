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
	private int nodes =0;
    UnsatPaths paths;
    EntityType[] candidates;
	Map<String, Double> succCount;
	HashMap<EntityType, Set<ConstraintPath>> dep;
    static final double C1 = 3;
    static final double C2 = 1;
	double best=Double.MAX_VALUE;
    int MAX_SUG = 5;
//    UnsatPaths unsatPaths;
    
    public HeuristicSearch(UnsatPaths paths, Set<EntityType> candidates, Map<String, Double> succCount) {
    	this.paths = paths;
    	this.candidates = (EntityType[])candidates.toArray();
    	this.succCount = succCount;
    	dep = getDependency();
    }

    // estimate the "cost" of satisfying remaining paths
    public int Estimate(Collection<ConstraintPath> paths, int index) {
    	
//    	return 0; // version 1: no guidance at all
 
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
    		
    		addSerchNode(heap, set, remaining, i+1);
    	}
    	
    	while (!heap.isEmpty()) {
    		FibonacciHeapNode<SearchNode> minnode = heap.removeMin();
    		SearchNode data = minnode.getData();
    		double key = minnode.getKey();
    		Set<Integer> set = data.set;
    		List<ConstraintPath> toSat = data.remaining;
    		boolean stop = goalTest(ret, data, key);
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
        		        		
        		addSerchNode(heap, toadd, remaining, i+1);
        	}
    	}
    	
    	System.out.println("Nodes_expanded: "+nodes);
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
				System.out.println("key is "+key+" best is "+best);
				Set<EntityType> eset = new HashSet<EntityType>();
				for (Integer j:node.set) {
					eset.add(candidates[j]);
				}
				ret.add(eset);
				return false;
			}
			else {
//		    	System.out.println("Nodes expanded: "+nodes);
		    	return true;
			}
		}
    }
    
    // return true if the search is done
    public void addSerchNode (FibonacciHeap<SearchNode> heap, Set<Integer> set, List<ConstraintPath> remaining, int index) {
    	double succSum=0;
		for (Integer j : set) {
			succSum+=succCount.get(candidates[j].toString());
		}
		double real = HeuristicSearch.getScore(set.size(),succSum);
		double est = C1*Estimate(remaining, index);
		double key = real + est;
		
		FibonacciHeapNode<SearchNode> newnode = new FibonacciHeapNode<SearchNode>(new SearchNode(set, remaining, index), key);
		nodes ++;
		heap.insert(newnode, newnode.getKey());
    }
    
    public abstract Set<ConstraintPath> mapsTo (EntityType element);
    
    public static double getScore(int setsize, double succ) {
    	return C1*setsize + C2*succ;
    }

}
