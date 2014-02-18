package diagnostic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import util.HeuristicSearch;
import diagnostic.explanation.Entity;
import diagnostic.explanation.Explanation;

/**
 * Super class of all explanation inference algorithms.
 */
public abstract class InferenceEngine {
	
	protected UnsatPaths paths;
	
	/**
	 * @param paths A set of unsatisfiable paths (errors) to be explained
	 */
	public InferenceEngine(UnsatPaths paths) {
		this.paths = paths;
	}
	
	/**
	 * @return Best explanation of the errors observed in constraint graph
	 */
	public String infer (boolean isConsole, boolean verbose) {
    	final Set<Entity> cand = getCandidates();
    	HeuristicSearch algorithm = getAlogithm(cand);
    	
    	StringBuffer sb = new StringBuffer();
		long startTime = System.currentTimeMillis();
		Set<Explanation> results = algorithm.AStarSearch();
		long endTime =  System.currentTimeMillis();
		if (verbose)
			System.out.println("ranking_time: "+(endTime-startTime));
		
		if (!isConsole)
			sb.append("\n"+HTMLinfo());
		else
			sb.append(info());

		List<Explanation> result = new ArrayList<Explanation>();
		for (Explanation set : results) {
			result.add(set);
		}
		Collections.sort(result);

		double best=Double.MAX_VALUE;
		int i=0;
		if (!isConsole)
			sb.append("<UL>\n");
		for ( ; i<result.size(); i++) {
			if (result.get(i).getWeight()>best)
				break;
			best = result.get(i).getWeight();
			if (isConsole)
				sb.append("- " + result.get(i).toConsole()+"\n");
			else
				sb.append("<LI> "+result.get(i).toHTML());
		}
		if (!isConsole)
			sb.append("</UL>\n");
		if (verbose)
			System.out.println("top_rank_size: "+i);
		if (i < result.size()) {
			if (!isConsole) {
				sb.append("<button onclick=\"show_more_expr()\">show/hide more</button><br>\n");
				sb.append("<div id=\"more_expr\">");
				for (; i < result.size(); i++) {
					sb.append(result.get(i).toHTML());
				}
				sb.append("</div>\n");
			} else {
				sb.append("Other less likely suggestions: \n");
				for (; i < result.size(); i++) {
					sb.append(result.get(i).toConsole());
				}
			}
		}
		return sb.toString();
	}
	
	/**
	 * @return A set of entities that the inference is performed on
	 */
	public abstract Set<Entity> getCandidates();
	
	/**
	 * @param candidates Basic elements of possible explanations
	 * @return Return an instance of heuristic search algorithm to use
	 */
	public abstract HeuristicSearch getAlogithm (Set<Entity> candidates);
	
	/**
	 * @return A string describing the nature of returned entities in HTML format
	 */
	public abstract String HTMLinfo ();	
	
	/**
	 * @return A string describing the nature of returned entities in plain text format
	 */
	public abstract String info ();	
}
