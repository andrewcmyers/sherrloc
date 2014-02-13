package diagnostic;

import graph.ConstraintPath;
import graph.Node;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import util.EntityExplanationFinder;
import util.HeuristicSearch;
import constraint.ast.Element;
import diagnostic.explanation.Entity;
import diagnostic.explanation.ExprEntity;

public class ExprInfer extends InferenceEngine {
	Map<String, Integer> succCount;
	
	public ExprInfer(UnsatPaths paths, Map<String, Integer> succCount) {
		super(paths);
		this.succCount = succCount;
	}
	
	@Override
	public Set<Entity> getCandidates() {
    	Set<Entity> cand = new HashSet<Entity>();
		
    	for (ConstraintPath path : paths.errPaths) {
    		for (Node n : path.getAllNodes()) {
    			Element e = n.getElement();
    			if (!e.getPosition().isEmpty())
    				cand.add(new ExprEntity(n.toString(), e.toSnippetString(), 
    						e.getPosition().toString(), succCount.get(n.toString())));
    		}
    	}
    	
    	return cand;
	}
	
	@Override
	public HeuristicSearch getAlogithm(Set<Entity> candidates) {
    	Entity[] candarr = candidates.toArray(new Entity[candidates.size()]);
		return new EntityExplanationFinder(paths, candarr);
	}
	
	@Override
	public String HTMLinfo() {
		return "<H4>Expressions in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n";
	}	
}