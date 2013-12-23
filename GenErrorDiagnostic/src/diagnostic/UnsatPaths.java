package diagnostic;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import util.CombinedExplainationFinder;
import util.HTTPUtil;
import util.HeuristicSearch;
import util.MinCutFinder;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.ast.Position;
import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.EquationEdge;
import constraint.graph.Node;

/* a set of unsatisfiable paths identified on the constraint graph */
public class UnsatPaths {
	Set<ConstraintPath> errPaths;
	// Reuse graph.env join env if the current env is already seen before
	Map<Environment, Environment> cachedEnv;	
	
	public UnsatPaths( ) {
        errPaths = new HashSet<ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
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
		
	// number of missing hypotheses, used for unit test
    public int getAssumptionNumber () {
        Set<Set<Hypothesis>> result = MissingHypoInfer.genAssumptions(this, cachedEnv);
    	return result.size();
    }
    
    public String getAssumptionString () {
        Set<Set<Hypothesis>> result = MissingHypoInfer.genAssumptions(this, cachedEnv);
        StringBuffer sb = new StringBuffer();
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
  	
    /* Calculating a min cut is NP complete. Currently, we use iterative deeping search to quickly identify the goal */
    public Set<Set<EquationEdge>> genEdgeCuts ( ) {
    	
    	MinCutFinder<EquationEdge> cutFinder = new MinCutFinder<EquationEdge>(this) {
			@Override
			public Set<EquationEdge> mapsTo(ConstraintPath path) {
				Set<EquationEdge> ret = new HashSet<EquationEdge>();
				for (Edge edge : path.getEdges()) {
					if (edge instanceof EquationEdge)
						ret.add((EquationEdge)edge);
				}
				return ret;
			}
		};
    	
    	return cutFinder.findMinCut( );
    }
    
    public Set<Set<String>> genSnippetCut () {
    	return genSnippetCut (6);
    }
    
    public Set<Set<String>> genHeuristicSnippetCut (Map<String, Double> succCount) {
    	Set<String> cand = new HashSet<String>();
		
    	for (ConstraintPath path : errPaths) {
    		for (Node n : path.getAllNodes()) {
    			if (!((ElementNode)n).getElement().getPosition().isEmpty())
    				cand.add(n.toString());
    		}
    	}
    	
    	HeuristicSearch<String> finder = new HeuristicSearch<String>(this, cand, succCount) {
    		@Override
    		public Set<ConstraintPath> mapsTo(String element) {
    			Set<ConstraintPath> ret = new HashSet<ConstraintPath>();
    			
    	    	for (ConstraintPath path : errPaths) {
    	    		for (Node n : path.getAllNodes()) {
    	    			if (n.toString().equals(element))
    	    				ret.add(path);
    	    		}
    	    	}
    			return ret;
    		}
		};
		
		return finder.AStarSearch();
    }
    
    public Set<Set<String>> genSnippetCut (int max) {
    	
    	MinCutFinder<String> cutFinder = new MinCutFinder<String>( this, max) {
			@Override
			public Set<String> mapsTo(ConstraintPath path) {
				Set<String> ret = new HashSet<String>();
				
				for (Node n : path.getAllNodes()) {
					ret.add(n.toString());
				}
				return ret;
			}
		};
    	
    	return cutFinder.findMinCut();
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
				path_buff.append("['pathelement', \'"+((ElementNode)n).getElement().getPosition()+"\'], ");
			}
			sb.append("<LI>\n<span class=\"path\" ");
			HTTPUtil.setShowHideActions(true, sb, path_buff.toString(), 0);
			sb.append(">");
			sb.append("A value with type "+path.getFirstElement().toDetailString() + 
				    " is being used at type " + path.getLastElement().toDetailString());
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
    
    public String genMissingAssumptions (Position pos, String sourceName ) {
    	StringBuffer sb = new StringBuffer();
		Set<Set<Hypothesis>> result = MissingHypoInfer.genAssumptions(this, cachedEnv);
		sb.append("<H3>Likely missing assumption(s): </H3>\n");
		sb.append("<UL>\n");
		
		for (Set<Hypothesis> g : result) {
			sb.append("<LI>");
			for (Hypothesis h : g) {
				sb.append( h.toString() + ";");
			}
			sb.append("\n");
		}

		sb.append("</UL>");
		return sb.toString();
	}
    
    public String genNodeCut (Map<String, Double> succCount, Map<String, Node> exprMap, boolean console) {
    	StringBuffer sb = new StringBuffer();
		long startTime = System.currentTimeMillis();
		Set<Set<String>> results = genHeuristicSnippetCut(succCount);
		long endTime =  System.currentTimeMillis();
		System.out.println("ranking_time: "+(endTime-startTime));
		
		if (!console)
			sb.append("<H4>Expressions in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n");

		List<ExprSuggestion> cuts = new ArrayList<ExprSuggestion>();
		int count = 1;
		for (Set<String> set : results) {
			cuts.add(new ExprSuggestion(count, set, succCount));
			count++;
		}
		Collections.sort(cuts);

		double best=Double.MAX_VALUE;
		int i=0;
		for ( ; i<cuts.size(); i++) {
			if (cuts.get(i).rank>best)
				break;
			best = cuts.get(i).rank;
			if (console)
				sb.append(cuts.get(i).toConsole(exprMap)+"\n");
			else
				sb.append(cuts.get(i).toHTML(exprMap));
		}
		System.out.println("top_rank_size: "+i);
		if (!console) {
			sb.append("<button onclick=\"show_more_expr()\">show/hide more</button><br>\n");
			sb.append("<div id=\"more_expr\">");
			for (; i < cuts.size(); i++) {
				sb.append(cuts.get(i).toHTML(exprMap));
			}
			sb.append("</div>\n");
		}
		return sb.toString();
    }
    
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
    
    public String genEdgeCut ( ) {
    	StringBuffer sb = new StringBuffer();
    	Set<Set<EquationEdge>> results = genEdgeCuts();
		sb.append("<H4>Constraints in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n");

		// sb.append("<OL>\n");

		// get all cuts and rank them
		List<ConsSuggestion> cuts = new ArrayList<ConsSuggestion>();
		int count = 1;
		for (Set<EquationEdge> set : results) {
			cuts.add(new ConsSuggestion(count, set));
			count++;
		}
		Collections.sort(cuts);

        int best=Integer.MAX_VALUE;
		int i=0;
		for ( ; i<cuts.size(); i++) {
			if (cuts.get(i).rank>best)
				break;
			best = cuts.get(i).rank;
			sb.append(cuts.get(i).toHTML());
		}
		sb.append("<button onclick=\"show_more_cons()\">show/hide more</button><br>\n");
		sb.append("<div id=\"more_cons\">");
		for ( ; i<cuts.size(); i++) {
			sb.append(cuts.get(i).toHTML());
		}
		sb.append("</div>\n");
		// sb.append("</OL>\n");
	
		return sb.toString();
    }
}
