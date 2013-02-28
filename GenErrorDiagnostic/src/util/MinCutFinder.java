package util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

// we do an iterative deeping search until at least one cut is returned
public class MinCutFinder {

    static int REC_MAX = 3;
    
    // find a minimal subset of candidates which covers dependencies.keySet
    public static <CandType, EleType> Set<Set<CandType>> findMinCut (Set<CandType> candidates, HashMap<EleType, Set<CandType>> dependencies) {
    	Set<Set<CandType>> ret = new HashSet<Set<CandType>>();
    	
    	// we do an iterative deeping search until at least one cut is returned
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, (CandType[])candidates.toArray(), 0, dependencies, new HashSet<CandType>(), ret);
   			if (ret.size()!=0)
   				break;
    	}
    	
    	return ret;
    }
    
    private static <CandType, EleType> void boundedDepthSearch (int level, CandType[] candidates, int index, HashMap<EleType, Set<CandType>> dependencies, Set<CandType> visited, Set<Set<CandType>> results) {
    	
    	/* first level */
   		for (int i=index; i<candidates.length; i++) {
   			CandType e = candidates[i];
   			visited.add(e);
   			
   			if (level == 1) {
   				boolean iscut = true;
   				
   				// for any path, at least one element in the visited list should appear
   	   			for (EleType goal : dependencies.keySet()) {
   	   				boolean flag = false;
   	   				for (CandType cand : visited) {
   	   					if (dependencies.get(goal).contains(cand)) {
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
   	   				HashSet<CandType> s = new HashSet<CandType>();
   	   				for (CandType v : visited) 
   	   					s.add(v);
   	   				results.add(s);
   	   			}
   			}
   			else
   				boundedDepthSearch (level-1, candidates, index+1, dependencies, visited, results);
   			
   			visited.remove(e);
   		}
    }

}
