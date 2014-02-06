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
import diagnostic.Entity;
import diagnostic.ExprSuggestion;
import diagnostic.UnsatPaths;

// we do an iterative deeping search until at least one cut is returned
public abstract class CombinedExplainationFinder {

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
    
    public abstract Set<Entity>  mapsTo (ConstraintPath path);
    
    public Set<Entity> genCandidates ( ) {
    	Set<Entity> ret = new HashSet<Entity>();
    	for (ConstraintPath path : paths.getPaths()) {
    		ret.addAll(mapsTo(path));
    	}
    	return ret;
    }
    
    public HashMap<ConstraintPath, Set<Entity>> getDependency ( ) {
    	HashMap<ConstraintPath, Set<Entity>> map = new HashMap<ConstraintPath, Set<Entity>>();
    	
    	for (ConstraintPath path : paths.getPaths()) {
    		map.put(path, mapsTo(path));
    	}
    	return map;
    }
    
    // find a minimal subset of candidates which covers dependencies.keySet
    public List<CombinedSuggestion<Entity>> findMinCut ( ) {
    	List<CombinedSuggestion<Entity>> ret = new ArrayList<CombinedSuggestion<Entity>>();
    	HashMap<ConstraintPath, Set<Entity>> dependencies = getDependency();
    	Set<Entity> candidates = genCandidates( );
    	Entity[] candArray = (Entity[])candidates.toArray();
    	// we do an iterative deeping search until at least one cut is returned
    	
    	UnsatPaths toTest = new UnsatPaths();
    	for (ConstraintPath path : dependencies.keySet()) {
			toTest.addUnsatPath(path);
		}
		Set<ExprSuggestion> r = toTest.genSnippetCut(1);
		for (ExprSuggestion sugg : r) {
			Set<String> sset = new HashSet<String>();
			for (Entity en : sugg.getEntities())
				sset.add(en.toString());
			ret.add(new CombinedSuggestion<Entity>(new HashSet<Entity>(), sset, succCount));
		}
    	int size = ret.size();
				
    	for (int level=1; level <= REC_MAX; level++) {
   			boundedDepthSearch (level, level, candArray, 0, dependencies, new ArrayList<Entity>(), ret);
   			if (ret.size()!=size)
   				break;
    	}
    	
    	return ret;
    }
    
    private void boundedDepthSearch (int level, int maxsize, Entity[] candidates, int index, HashMap<ConstraintPath, Set<Entity>> dependencies, List<Entity> visited, List<CombinedSuggestion<Entity>> results) {
    	
    	/* first level */
   		for (int i=index; i<candidates.length; i++) {
   			Entity e = candidates[i];
   			visited.add(e);
   			
   			if (level == 1) {
   				List<ConstraintPath> remaining = new ArrayList<ConstraintPath>();
   				
   				// for any path, at least one element in the visited list should appear
   	   			for (ConstraintPath path : dependencies.keySet()) {
   	   				boolean flag = false;
   	   				for (Entity cand : visited) {
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
   	   			
   	   			HashSet<Entity> s = new HashSet<Entity>();
  				for (Entity v : visited) 
  					s.add(v);
   	   			if (remaining.isEmpty()) {
   	   				results.add(new CombinedSuggestion<Entity>(s, new HashSet<String>(), succCount));
   	   			}
   	   			else {
   	   				UnsatPaths toTest = new UnsatPaths();
   	   				for (ConstraintPath path : remaining)
   	   					toTest.addUnsatPath(path);
   	   				Set<ExprSuggestion> r = toTest.genSnippetCut(maxsize-s.size()+1);
   	   				for (ExprSuggestion sugg : r) {
   	   					Set<String> sset = new HashSet<String>();
   	   					for (Entity en : sugg.getEntities())
   	   						sset.add(en.toString());
//   	   					ExprSuggestion sugg = new ExprSuggestion(0, set, succCount);
//   	   					if (sugg.getRank()<SAT_MAX) {
 	   					results.add(new CombinedSuggestion<Entity>(s, sset, succCount));
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
