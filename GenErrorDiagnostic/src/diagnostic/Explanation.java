package diagnostic;

import graph.Node;

import java.util.Map;
import java.util.Set;

import util.HTTPUtil;

/**
 * <code>Explanation</code> is the result of the error diagnosis algorithm. An
 * explanation consists of a set of entities (e.g., expressions, constraints,
 * hypotheses), a weight w.r.t. ranking metric, and number of satisfiable paths
 * using those entities
 */
public class Explanation implements Comparable<Explanation> {
	private final double weight;
	private Set<Entity> entities;
	private boolean DEBUG = false;

	public Explanation(Set<Entity> entities, double weight) {
		this.entities = entities;
		this.weight = weight;
	}
	
	public double getWeight() {
		return weight;
	}
	
	public Set<Entity> getEntities() {
		return entities;
	}
	
	@Override
	public int compareTo(Explanation o) {
		return new Double(weight).compareTo(o.weight);
	}
	
	public String toHTML (Map<String, Node> exprMap) {
		StringBuffer sb = new StringBuffer();
		
		if (DEBUG) {
			sb.append("<span class=\"rank\">(score "+weight+")</span> ");
		}
		
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
		for (Entity en : entities) {
			en.toHTML(exprMap, locBuffer, exprBuffer);
    	}
    	sb.append("<span class=\"path\" ");
		HTTPUtil.setShowHideActions(false, sb, locBuffer.toString(), 0);
		sb.append(">");
		sb.append(exprBuffer.toString()+"</span>");
    	sb.append("<button onclick=\"hide_all();show_elements_perm(true, [");
        sb.append(locBuffer.toString());
    	sb.append("])\" ");
		// setShowHideActions(true, sb, path_buff.toString(), 0);
		sb.append(">show it</button><br>\n");
    	
   		return sb.toString();
	}
	
	public String toConsole (Map<String, Node> exprMap) {
		StringBuffer sb = new StringBuffer();
		
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
		for (Entity en : entities) {
			en.toConsole(exprMap, locBuffer, exprBuffer);
    	}
		sb.append(exprBuffer.toString()+": ");
        sb.append(locBuffer.toString());
    	
   		return sb.toString();
	}
	
	public String toString() {
    	StringBuffer exprBuffer = new StringBuffer();
    	for (Entity e : entities) {
    		exprBuffer.append(e.toString()+e.getSuccCount()+"    ");
    	}
    	return exprBuffer.toString();
	}
}
