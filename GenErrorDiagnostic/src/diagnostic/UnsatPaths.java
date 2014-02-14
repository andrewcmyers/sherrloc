package diagnostic;

import graph.ConstraintPath;
import graph.Node;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.HTTPUtil;
import util.MinCutFinder;
import constraint.ast.Element;
import diagnostic.explanation.Entity;
import diagnostic.explanation.Explanation;
import diagnostic.explanation.ExprEntity;

/* a set of unsatisfiable paths identified on the constraint graph */
public class UnsatPaths {
	Set<ConstraintPath> errPaths;
	
	public UnsatPaths( ) {
        errPaths = new HashSet<ConstraintPath>();
    }
	
	public void addUnsatPath (ConstraintPath path) {
		errPaths.add(path);		
	}
	
	public int size () {
		return errPaths.size();
	}
	
	public Collection<ConstraintPath> getPaths () {
		return errPaths;
	}
	
	public Set<Node> getAllNodes () {
    	Set<Node> ret = new HashSet<Node>();
    	
		for (ConstraintPath path : errPaths) {
    		for (Node n : path.getAllNodes()) {
    			ret.add(n);
    		}
    	}
		return ret;
	}
  	    
    public Set<Explanation> genSnippetCut (int max) {
    	Set<Entity> cand = new HashSet<Entity>();
		
    	for (ConstraintPath path : errPaths) {
    		for (Node n : path.getAllNodes()) {
    			Element e = n.getElement();
    			if (!e.getPosition().isEmpty())
    				cand.add(new ExprEntity(n.toString(), e.toSnippetString(), 
    						e.getPosition().toString(), 0));
    		}
    	}
    	
    	Entity[] candarr = cand.toArray(new Entity[cand.size()]);
    	MinCutFinder cutFinder = new MinCutFinder(this, candarr);
    	
    	return cutFinder.AStarSearch();
    }
    
    
    public String toHTML ( ) {
    	StringBuffer sb = new StringBuffer();
    	sb.append("<H3>");
    	sb.append(size() +" type mismatch" + (size() == 1 ? "" : "es") + " found ");
        sb.append("<button onclick=\"show_all_errors()\">show more details</button><br>\n");
        sb.append("</H3>");
        sb.append("<div id=\"all_error\">");


		sb.append("<UL>\n");
		for (ConstraintPath path : errPaths) {
			StringBuffer path_buff = new StringBuffer();
			List<Node> nodes = path.getIdNodes();
			for (Node n : nodes) {
				path_buff.append("['pathelement', \'"+n.getElement().getPosition()+"\'], ");
			}
			sb.append("<LI>\n<span class=\"path\" ");
			HTTPUtil.setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">");
			sb.append("A value with type "+path.getFirstElement() + 
				    " is being used at type " + path.getLastElement());
			sb.append("</span>\n");
        		sb.append("<button onclick=\"hide_all();show_elements_perm(true, [");
	        	sb.append(path_buff.toString());
        		sb.append("])\" ");
			// setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">show it</button><br>\n");
		}
		sb.append("</UL>\n");
        sb.append("</div>\n");
		return sb.toString();
    }
    
    public String genMissingAssumptions (String sourceName ) {
    	StringBuffer sb = new StringBuffer();
    	MissingHypoInfer infer = new MissingHypoInfer(this);
		Set<Explanation> result = infer.infer();
		sb.append("<H3>Likely missing assumption(s): </H3>\n");
		sb.append("<UL>\n");
		
		for (Explanation g : result) {
			sb.append("<LI>");
			for (Entity en : g.getEntities()) {
				en.toHTML(null, sb);
			}
			sb.append("\n");
		}

		sb.append("</UL>");
		return sb.toString();
	}
    
    /*
    public String genCombinedResult (Map<Environment, Environment> envs, Map<String, Node> exprMap, Map<String, Double> succCount) {
    	final Set<Hypothesis> candidates = new HashSet<Hypothesis>();
    	final Map<Environment, Environment> cachedEnv = envs;
    	StringBuffer sb = new StringBuffer();
    	for (ConstraintPath path : getPaths())
    		candidates.add(path.getMinHypo());
    	    	    	
    	CombinedExplainationFinder<Hypothesis> cutFinder = new CombinedExplainationFinder<Hypothesis>(this, exprMap, succCount) {
			@Override
			public Set<Hypothesis> mapsTo(ConstraintPath path) {
				return MissingHypoInfer.genAssumptionDep(path, candidates, cachedEnv);
			}
		};
 
		List<CombinedSuggestion<Hypothesis>> result = cutFinder.findMinCut();
 		Collections.sort(result);

		int best = Integer.MAX_VALUE;
		int i = 0;
		for (; i < result.size(); i++) {
//			if (result.get(i).rank > best)
//				break;
			best = result.get(i).rank;
			sb.append(result.get(i).toHTML(exprMap));
			System.out.println("top_rank_size: " + i);
			sb.append("<button onclick=\"show_more_expr()\">show/hide more</button><br>\n");
			sb.append("<div id=\"more_expr\">");
			for (; i < result.size(); i++) {
				sb.append(result.get(i).toHTML(exprMap));
			}
			sb.append("</div>\n");
		}
		return sb.toString();
    }
    */
}
