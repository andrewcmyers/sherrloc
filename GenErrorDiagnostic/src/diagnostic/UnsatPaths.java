package diagnostic;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import util.HTTPUtil;
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

public class UnsatPaths {
	// source and sink of the unsatisfiable paths. 
	// This set is filled by function genErrorPaths, and used by genAssumptions
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
	
//	public ConstraintPath getPath (AttemptGoal goal) {
//		return errPaths.get(goal);
//	}
	
//	public Collection<AttemptGoal> getAttemptGoals () {
//		return errPaths.keySet();
//	}
	
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
    
    public List<UnsatPaths> genIndependentPaths ( ) {
    	List<UnsatPaths> ret = new ArrayList<UnsatPaths>();
//    	for (ConstraintPath cpath : errPaths) {
//    		List<UnsatPaths> matched = new ArrayList<UnsatPaths>();
//    		for (UnsatPaths path : ret) {
//    			for (ConstraintPath onepath : path.errPaths) {
//    				if (onepath.intersects(cpath))
//    					matched.add(path);
//    			}
//    		}
//    		
//    		if (matched.size()==0) { 
//    			UnsatPaths newpath = new UnsatPaths();
//    			newpath.addUnsatPath(cpath);
//    			ret.add(newpath);
//    		}
//    		else if(matched.size()==1) {
//    			matched.get(0).addUnsatPath(cpath);
//    		}
//    		// the most complicated case when the current path unions multiple previously separate paths
//    		else {
//    			UnsatPaths newpath = new UnsatPaths();
//    			newpath.addUnsatPath(cpath);
//    			for (UnsatPaths mpath : matched) {
//    				for (ConstraintPath mcpath : mpath.getPaths()) {
//    					newpath.addUnsatPath(mcpath);
//    					ret.remove(mpath);
//    				}
//    			}
//    			ret.add(newpath);
//    		}
//    	}
    	ret.add(this);
    	return ret;
//    	List<UnsatPaths> groups = new ArrayList<UnsatPaths>();
//    	groups.add(this);
//    	List<Set<Node>> groupnodes = new ArrayList<Set<Node>>();
//    	
//    	for (ConstraintPath cpath : errPaths) {
//        	// first, check if some group intersects with the current path
//    		boolean matched = false;
//    		for (int i=0; i<groups.size(); i++) {
//    			List<Node> ln = cpath.getAllNodes();
//    			for (Node n : ln) {
//    				if (groupnodes.get(i).contains(n)) {
//    					matched = true;
//    					groups.get(i).addUnsatPath(cpath);
//    					groupnodes.get(i).addAll(ln);
//    					break;
//    				}
//    			}
//    			if (matched)
//    				break;
//    		}
//    		
//    		if (!matched) {
//    			UnsatPaths p = new UnsatPaths();
//    			p.addUnsatPath(cpath);
//    			groups.add(p);
//    			groupnodes.add(new HashSet<Node>(cpath.getAllNodes()));
//    		}
//    	}
//    	return groups;

//    		List<UnsatPaths> matched = new ArrayList<UnsatPaths>();
//    		for (UnsatPaths group : ret) {
//    			for (AttemptGoal g1 : group.keySet()) {
//    				if (errPaths.get(goal).intersects(group.get(g1)))
//    					matched.add(group);
//    			}
//    		}
//    		if (matched.size()==0) {
//    			HashMap<AttemptGoal, ConstraintPath> newmap = new HashMap<AttemptGoal, ConstraintPath>();
//    			newmap.put(goal, errPaths.get(goal));    			
//    			ret.add(newmap);
//    		}
//    		else if(matched.size()==1) {
//    			matched.get(0).put(goal, errPaths.get(goal));
//    		}
//    		// the most complicated case when the current path unions multiple previously separate paths
//    		else {
//    			HashMap<AttemptGoal, ConstraintPath> newmap = new HashMap<AttemptGoal, ConstraintPath>();
//    			newmap.put(goal, errPaths.get(goal));    			
//    			for (HashMap<AttemptGoal, ConstraintPath> map : matched) {
//    				newmap.putAll(map);
//    				ret.remove(map);
//    			}
//    			ret.add(newmap);
//    		}
//    	}
//    	return ret;
    }
	
    /* Calculating a min cut is NP complete. Currently, we use iterative deeping search to quickly identify the goal */
    public Set<Set<EquationEdge>> genEdgeCuts ( ) {
//    	HashSet<EquationEdge> candidates = new HashSet<EquationEdge>();
////    	HashMap<AttemptGoal, Set<EquationEdge>> map = new HashMap<AttemptGoal, Set<EquationEdge>>();
//
//    	for (ConstraintPath path : errPaths) {
////    		Set<EquationEdge> set = new HashSet<EquationEdge>();
//    		for (Edge e : path.getEdges()) {
//    			if (e instanceof EquationEdge) {
//    				EquationEdge ee = (EquationEdge) e;
//    				if (!ee.getEquation().getFirstElement().toDetailString().equals(ee.getEquation().getSecondElement().toDetailString())) {
////    					set.add(ee);
//    					candidates.add(ee);
//    				}
//    			}
//    		}
////    		map.put(goal, set);
//    	}
    	
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
    
    public Set<Set<String>> genSnippetCut ( ) {
//    	HashSet<String> candidates = new HashSet<String>();
////    	HashMap<AttemptGoal, Set<String>> map = new HashMap<AttemptGoal, Set<String>>();
//
//    	for (ConstraintPath path : errPaths) {
//    		Set<String> set = new HashSet<String>();
//    		for (Node n : path.getAllNodes()) {
//    			if (((ElementNode)n).isInCons()) {
////    				set.add(n.toString());
//    				candidates.add(n.toString());
//    			}
//    		}
////    		map.put(goal, new HashSet<String>(set));
//    	}
    	
    	MinCutFinder<String> cutFinder = new MinCutFinder<String>( this) {
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
    
    public Set<Set<Element>> genElementCut ( ) {
    	MinCutFinder<Element> cutFinder = new MinCutFinder<Element>( this) {
			@Override
			public Set<Element> mapsTo(ConstraintPath path) {
				Set<Element> ret = new HashSet<Element>();
				
				for (Node n : path.getAllNodes()) {
					ret.add(((ElementNode)n).getElement().getBaseElement());
				}
				return ret;
			}
		};
    	
    	return cutFinder.findMinCut();
	}
    
    public String toHTML ( ) {
    	StringBuffer sb = new StringBuffer();
    	sb.append("<H3>");
    	sb.append(size() +" type mismatch" + (size() == 1 ? "" : "s") + " found: \n");
		sb.append("</H3>\n");
		sb.append("<UL>\n");
		for (ConstraintPath path : errPaths) {
			//sb.append("<div class=\"moreinfo\"> " +
			//		errorPaths.get(goal).toString() + " \n");
			//sb.append("</div>);
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
		return sb.toString();
    }
    
    public String genMissingAssumptions (Position pos, String sourceName ) {
    	StringBuffer sb = new StringBuffer();
		Set<Hypothesis> result = MissingHypoInfer.genAssumptions(
				this, cachedEnv).iterator().next();
		sb.append("<H3>Likely missing assumption(s): </H3>\n");
		sb.append("<UL>\n");
		sb.append("<LI>");
		sb.append("<OL>\n");

		String missingCons = "";
		for (Hypothesis g : result)
			missingCons += g.toString() + ";";
		sb.append(missingCons + "\n");
		sb.append("</OL>\n");

		if (pos != null) {
			sb.append("\n<pre class=\"code\">\n");
			try {
				FileReader fstream = new FileReader(sourceName);
				BufferedReader in = new BufferedReader(fstream);
				String current;
				int currentline = 0;
				while ((current = in.readLine()) != null) {
					currentline++;
					if (currentline < pos.getLineStart())
						continue;

					if (pos.getLineStart() == currentline) {
						current = current
								+ " <span class=\"missingConstraint\"> "
								+ missingCons + "</span>";
					}

					sb.append(currentline + ". " + current + "\n");
				}
				in.close();
			} catch (IOException e) {
				sb.append("Failed to read file: " + sourceName);
			}
			sb.append("</pre>\n");
		}
		sb.append("</UL>");
		return sb.toString();
	}
    
    public String genNodeCut (Map<String, Double> succCount, Map<String, Node> exprMap) {
    	StringBuffer sb = new StringBuffer();
    	Set<Set<String>> results = genSnippetCut();
		long startTime = System.currentTimeMillis();
		long endTime =  System.currentTimeMillis();
		System.out.println("ranking_time: "+(endTime-startTime));
		
		sb.append("<H4>Expressions in the source code that appear most likely to be wrong (mouse over to highlight code):</H4>\n");

		List<ExprSuggestion> cuts = new ArrayList<ExprSuggestion>();
		int count = 1;
		for (Set<String> set : results) {
			cuts.add(new ExprSuggestion(count, set, succCount));
			count++;
		}
		Collections.sort(cuts);

		int best=Integer.MAX_VALUE;
		int i=0;
		for ( ; i<cuts.size(); i++) {
			if (cuts.get(i).rank>best)
				break;
			best = cuts.get(i).rank;
			sb.append(cuts.get(i).toHTML(exprMap));
		}
		System.out.println("top_rank_size: "+i);
		sb.append("<button onclick=\"show_more_expr()\">show/hide more</button><br>\n");
		sb.append("<div id=\"more_expr\">");
		for ( ; i<cuts.size(); i++) {
			sb.append(cuts.get(i).toHTML(exprMap));
		}
		sb.append("</div>\n");
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
