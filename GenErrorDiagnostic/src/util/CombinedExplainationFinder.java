package util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.graph.ConstraintPath;
import constraint.graph.Node;
import diagnostic.CombinedSuggestion;
import diagnostic.ExprSuggestion;
import diagnostic.UnsatPaths;

// we do an iterative deeping search until at least one cut is returned
public abstract class CombinedExplainationFinder<EntityType> {

    int REC_MAX = 6;
//    int SAT_MAX = 4;
    UnsatPaths paths;
	Map<String, Node> exprMap;
	Map<String, Double> succCount;

    
    public CombinedExplainationFinder(UnsatPaths paths, Map<String, Node> exprMap, Map<String, Double> succCount) {
		this.paths = paths;
		this.exprMap = exprMap;
		this.succCount = succCount;
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
    		map.put(path, mapsTo(path));
    	}
    	return map;
    }
    
    // find a minimal subset of candidates which covers dependencies.keySet
    public List<CombinedSuggestion<EntityType>> findMinCut ( ) {
    	List<CombinedSuggestion<EntityType>> ret = new ArrayList<CombinedSuggestion<EntityType>>();
    	HashMap<ConstraintPath, Set<EntityType>> dependencies = getDependency();
    	Set<EntityType> candidates = genCandidates( );
    	EntityType[] candArray = (EntityType[])candidates.toArray();
    	// we do an iterative deeping search until at least one cut is returned
    	
    	UnsatPaths toTest = new UnsatPaths();
    	for (ConstraintPath path : dependencies.keySet()) {
			toTest.addUnsatPath(path);
		}
		Set<Set<String>> r = toTest.genSnippetCut(1);
		for (Set<String> set : r) {
			ret.add(new CombinedSuggestion<EntityType>(new HashSet<EntityType>(), set, succCount));
		}
    	int size = ret.size();
				
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, level, candArray, 0, dependencies, new ArrayList<EntityType>(), ret);
   			if (ret.size()!=size)
   				break;
    	}
    	
    	return ret;
    }
    
    private void boundedDepthSearch (int level, int maxsize, EntityType[] candidates, int index, HashMap<ConstraintPath, Set<EntityType>> dependencies, List<EntityType> visited, List<CombinedSuggestion<EntityType>> results) {
    	
    	/* first level */
   		for (int i=index; i<candidates.length; i++) {
   			EntityType e = candidates[i];
   			visited.add(e);
   			
   			if (level == 1) {
   				List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
   				
   				// for any path, at least one element in the visited list should appear
   	   			for (ConstraintPath path : dependencies.keySet()) {
   	   				boolean flag = false;
   	   				for (EntityType cand : visited) {
   	   					if (dependencies.get(path).contains(cand)) {
   	   						flag=true;
   	   						break;
   	   					}
   	   				}
   	   				if (!flag) {
   	   					remaining.add(path);
   	   				}
   	   			}
   	   			
//   	   			if (!remaining.isEmpty() && iscut) {
//   	   				for (ConstraintPath path : remaining) {
//   	   					System.out.println(path);
//   	   					System.exit(0);
//   	   				}
//   	   			}
   	   			
   	   			HashSet<EntityType> s = new HashSet<EntityType>();
  				for (EntityType v : visited) 
  					s.add(v);
   	   			if (remaining.isEmpty()) {
   	   				results.add(new CombinedSuggestion<EntityType>(s, new HashSet<String>(), succCount));
   	   			}
   	   			else {
   	   				UnsatPaths toTest = new UnsatPaths();
   	   				for (ConstraintPath path : remaining)
   	   					toTest.addUnsatPath(path);
   	   				Set<Set<String>> r = toTest.genSnippetCut(maxsize-s.size()+1);
   	   				for (Set<String> set : r) {
//   	   					ExprSuggestion sugg = new ExprSuggestion(0, set, succCount);
//   	   					if (sugg.getRank()<SAT_MAX) {
   	   						results.add(new CombinedSuggestion<EntityType>(s, set, succCount));
//   	   					}
   	   				}
   	   			}
   	 		}
   			else
   				boundedDepthSearch (level-1, maxsize, candidates, i+1, dependencies, visited, results);
   			
   			visited.remove(e);
   		}
    }

}
