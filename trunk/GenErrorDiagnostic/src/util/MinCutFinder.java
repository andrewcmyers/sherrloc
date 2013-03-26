package util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.graph.ConstraintPath;
import diagnostic.UnsatPaths;

// we do an iterative deeping search until at least one cut is returned
public abstract class MinCutFinder<EntityType> {

    static int REC_MAX = 6;
    UnsatPaths paths;
//    UnsatPaths unsatPaths;
    
    public MinCutFinder(UnsatPaths paths) {
		this.paths = paths;
    }
    
    public abstract Set<EntityType>  mapsTo (ConstraintPath path);
    
    public Set<EntityType> genCandidates ( ) {
    	Set<EntityType> ret = new HashSet<EntityType>();
    	for (ConstraintPath path : paths.getPaths()) {
    		ret.addAll(mapsTo(path));
    	}
    	return ret;
    }
    
    public HashMap<ConstraintPath, Set<EntityType>> getDependency ( ) {
    	HashMap<ConstraintPath, Set<EntityType>> map = new HashMap<ConstraintPath, Set<EntityType>>();
    	
    	for (ConstraintPath path : paths.getPaths()) {
//    		Set<EntityType> set = new HashSet<EntityType>();
//    		for (Edge e : unsatPaths.getPath(goal).getEdges()) {
//    			if (((ElementNode)n).isInCons()) {
//    				set.addAll(mapsTo(e));
//    			}
//    		}
    		map.put(path, mapsTo(path));
    	}
    	return map;
    }
    
    // find a minimal subset of candidates which covers dependencies.keySet
    public Set<Set<EntityType>> findMinCut ( ) {
    	Set<Set<EntityType>> ret = new HashSet<Set<EntityType>>();
    	HashMap<ConstraintPath, Set<EntityType>> dependencies = getDependency();
    	Set<EntityType> candidates = genCandidates( );
    	// we do an iterative deeping search until at least one cut is returned
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, (EntityType[])candidates.toArray(), 0, dependencies, new ArrayList<EntityType>(), ret);
   			if (ret.size()!=0)
   				break;
    	}
    	
    	return ret;
    }
    
    private void boundedDepthSearch (int level, EntityType[] candidates, int index, HashMap<ConstraintPath, Set<EntityType>> dependencies, List<EntityType> visited, Set<Set<EntityType>> results) {
    	
    	/* first level */
   		for (int i=index; i<candidates.length; i++) {
   			EntityType e = candidates[i];
   			visited.add(e);
   			
   			if (level == 1) {
   				boolean iscut = true;
   				
   				// for any path, at least one element in the visited list should appear
   	   			for (ConstraintPath path : dependencies.keySet()) {
   	   				boolean flag = false;
   	   				for (EntityType cand : visited) {
   	   					if (dependencies.get(path).contains(cand)) {
   	   						flag = true;
   	   						break;
   	   					}
   	   				}
   	   				if (!flag) {
   	   					iscut = false;
   	 	   	   			break;
   	   				}
   	    		}
   	   			
   	   			if (iscut) {
   	   				HashSet<EntityType> s = new HashSet<EntityType>();
   	   				for (EntityType v : visited) {
   	   					s.add(v);
   	   				}
   	   				results.add(s);
   	   			}
   			}
   			else
   				boundedDepthSearch (level-1, candidates, i+1, dependencies, visited, results);
   				visited.remove(e);
   		}
    }

}
