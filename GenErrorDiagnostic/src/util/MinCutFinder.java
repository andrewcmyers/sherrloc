package util;

import diagnostic.Entity;
import diagnostic.UnsatPaths;

/**
 * Fining minimal cut is an instance of the more general heuristic search with a
 * metric |E|
 */
public class MinCutFinder extends EntityExplanationFinder {
    
    public MinCutFinder(UnsatPaths paths, Entity[] candidates) {
    	super(paths, candidates, 1, 0);
    }
}
