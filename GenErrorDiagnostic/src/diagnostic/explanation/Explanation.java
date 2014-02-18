package diagnostic.explanation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import util.HTMLUtil;

/**
 * <code>Explanation</code> is the result of the error diagnosis algorithm. An
 * explanation consists of a set of entities (e.g., expressions, constraints,
 * hypotheses) and a weight w.r.t. ranking metric
 */
public class Explanation implements Comparable<Explanation> {
	private final double weight;
	private Set<Entity> entities;
	private boolean DEBUG = false;

	/**
	 * @param entities a set of entities (e.g., expressions, constraints, hypotheses)
	 * @param weight a weight w.r.t. ranking metric
	 */
	public Explanation(Set<Entity> entities, double weight) {
		this.entities = entities;
		this.weight = weight;
	}
	
	/**
	 * @return Weight
	 */
	public double getWeight() {
		return weight;
	}
	
	/**
	 * @return A set of entities
	 */
	public Set<Entity> getEntities() {
		return entities;
	}
	
	@Override
	public int compareTo(Explanation o) {
		return new Double(weight).compareTo(o.weight);
	}
	
	/**
	 * For pretty print of the explanation in HTML format
	 * 
	 * @return Explanation in HTML format
	 */
	public String toHTML ( ) {
		StringBuffer sb = new StringBuffer();
		
		if (DEBUG) {
			sb.append("<span class=\"rank\">(score "+weight+")</span> ");
		}
		
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
		for (Entity en : entities) {
			en.toHTML(locBuffer, exprBuffer);
    	}
    	sb.append("<span class=\"path\" ");
		HTMLUtil.setShowHideActions(false, sb, locBuffer.toString(), 0);
		sb.append(">");
		sb.append(exprBuffer.toString()+"</span>");
    	sb.append("<button onclick=\"hide_all();show_elements_perm(true, [");
        sb.append(locBuffer.toString());
    	sb.append("])\" ");
		sb.append(">show it</button><br>\n");
    	
   		return sb.toString();
	}
	
	/**
	 * For pretty print of the explanation to console
	 * 
	 * @return Explanation in plain text
	 */
	public String toConsole ( ) {
		StringBuffer sb = new StringBuffer();
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
    	List<String> list = new ArrayList<String>();

    	if (DEBUG) {
			sb.append("(score "+weight+") ");
		}
		for (Entity en : entities) {
			en.toConsole(locBuffer, exprBuffer);
			String loc = locBuffer.toString();
			list.add(exprBuffer.toString()+(loc.equals("")?"":":["+loc+"]"));
			locBuffer.setLength(0);
			exprBuffer.setLength(0);
    	}
		// set the order so that the result is deterministic. The main purpose
		// of doing this is for unit test
		Collections.sort(list);
		
		for (String str : list)
			sb.append(str + ";");
		sb.append("\n");
    	
   		return sb.toString();
	}

	@Override
	public String toString() {
    	StringBuffer exprBuffer = new StringBuffer();
    	for (Entity en : entities) {
    		exprBuffer.append(en.toString()+"("+en.getSuccCount()+")    ");
    	}
    	return exprBuffer.toString();
	}
}
