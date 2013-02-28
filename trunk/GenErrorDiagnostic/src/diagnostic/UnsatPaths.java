package diagnostic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.AttemptGoal;
import util.MinCutFinder;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.EquationEdge;
import constraint.graph.Node;

public class UnsatPaths {
	// source and sink of the unsatisfiable paths. 
	// This set is filled by function genErrorPaths, and used by genAssumptions
	HashMap<AttemptGoal, ConstraintPath> errPaths;
	// Reuse graph.env join env if the current env is already seen before
	HashMap<Environment, Environment> cachedEnv;	
	
	public UnsatPaths() {
        errPaths = new HashMap<AttemptGoal, ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
    }
	
	public void addUnsatPath (AttemptGoal goal, ConstraintPath path) {
		errPaths.put(goal, path);		
	}
	
	public int size () {
		return errPaths.size();
	}
	
	public Collection<ConstraintPath> getPaths () {
		return errPaths.values();
	}
	
    public int getAssumptionNumber () {
        Set<Set<Hypothesis>> result = MissingHypoInfer.genAssumptions(errPaths.keySet(), cachedEnv);
    	return result.size();
    }
    
    public String getAssumptionString () {
        Set<Set<Hypothesis>> result = MissingHypoInfer.genAssumptions(errPaths.keySet(), cachedEnv);
        StringBuffer sb = new StringBuffer();
        int counter = 0;
        for (Set<Hypothesis> s : result) {
            List<String> list = new ArrayList<String>();
        	for (Hypothesis g :s )
        		list.add(g.toString());
        	Collections.sort(list);
        	for (String str : list)
        		sb.append(str+";");
        	sb.append("\n");
        }
    	return sb.toString();
    }
    
    public List<HashMap<AttemptGoal, ConstraintPath>> genIndependentPaths ( ) {
    	List<HashMap<AttemptGoal, ConstraintPath>> ret = new ArrayList<HashMap<AttemptGoal, ConstraintPath>>();
    	for (AttemptGoal goal : errPaths.keySet()) {
    		List<HashMap<AttemptGoal, ConstraintPath>> matched = new ArrayList<HashMap<AttemptGoal,ConstraintPath>>();
    		for (HashMap<AttemptGoal, ConstraintPath> group : ret) {
    			for (AttemptGoal g1 : group.keySet()) {
    				if (errPaths.get(goal).intersects(group.get(g1)))
    					matched.add(group);
    			}
    		}
    		if (matched.size()==0) {
    			HashMap<AttemptGoal, ConstraintPath> newmap = new HashMap<AttemptGoal, ConstraintPath>();
    			newmap.put(goal, errPaths.get(goal));    			
    			ret.add(newmap);
    		}
    		else if(matched.size()==1) {
    			matched.get(0).put(goal, errPaths.get(goal));
    		}
    		// the most complicated case when the current path unions multiple previously separate paths
    		else {
    			HashMap<AttemptGoal, ConstraintPath> newmap = new HashMap<AttemptGoal, ConstraintPath>();
    			newmap.put(goal, errPaths.get(goal));    			
    			for (HashMap<AttemptGoal, ConstraintPath> map : matched) {
    				newmap.putAll(map);
    				ret.remove(map);
    			}
    			ret.add(newmap);
    		}
    	}
    	return ret;
    }
	
    /* Calculating a min cut is NP complete. Currently, we use iterative deeping search to quickly identify the goal */
    public Set<Set<EquationEdge>> genEdgeCuts (Set<AttemptGoal> remaining) {
    	HashSet<EquationEdge> candidates = new HashSet<EquationEdge>();
    	HashMap<AttemptGoal, Set<EquationEdge>> map = new HashMap<AttemptGoal, Set<EquationEdge>>();

    	for (AttemptGoal goal : remaining) {
    		Set<EquationEdge> set = new HashSet<EquationEdge>();
    		for (Edge e : errPaths.get(goal).getEdges()) {
    			if (e instanceof EquationEdge) {
    				EquationEdge ee = (EquationEdge) e;
    				if (!ee.getEquation().getFirstElement().toDetailString().equals(ee.getEquation().getSecondElement().toDetailString())) {
    					set.add(ee);
    					candidates.add(ee);
    				}
    			}
    		}
    		map.put(goal, set);
    	}
    	
    	return MinCutFinder.findMinCut(candidates, map);
    }
    
    public Set<Set<String>> genNodeCuts (Set<AttemptGoal> remaining) {
    	HashSet<String> candidates = new HashSet<String>();
    	HashMap<AttemptGoal, Set<String>> map = new HashMap<AttemptGoal, Set<String>>();

    	for (AttemptGoal goal : remaining) {
    		Set<String> set = new HashSet<String>();
    		for (Node n : errPaths.get(goal).getAllNodes()) {
    			if (((ElementNode)n).isInCons()) {
    				set.add(n.toString());
    				candidates.add(n.toString());
    			}
    		}
    		map.put(goal, new HashSet<String>(set));
    	}
    	
    	return MinCutFinder.findMinCut(candidates, map);
    }
}
