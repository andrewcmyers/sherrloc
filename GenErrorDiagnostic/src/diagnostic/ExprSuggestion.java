package diagnostic;

import java.util.Map;
import java.util.Set;

import util.HTTPUtil;
import constraint.graph.Node;

public class ExprSuggestion implements Comparable<ExprSuggestion> {
	private final double weight;
	private Set<Entity> entities;
	private final double succCount;

	public ExprSuggestion(Set<Entity> exprs, double succCount, double weight) {
		this.entities = exprs;
		this.weight = weight;
		this.succCount = succCount;
	}
	
	public double getWeight() {
		return weight;
	}
	
	public double getSuccCount() {
		return succCount;
	}
	
	public Set<Entity> getEntities() {
		return entities;
	}
	
	@Override
	public int compareTo(ExprSuggestion o) {
		return new Double(weight).compareTo(o.weight);
	}
	
	public String toHTML (Map<String, Node> exprMap) {
		StringBuffer sb = new StringBuffer();
//		sb.append("<LI>\n");
//		sb.append("<span class=\"rank\">(score "+rank+")</span> ");
		
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
	
	@Override
	public String toString() {
    	StringBuffer exprBuffer = new StringBuffer();
    	for (Entity e : entities) {
    		exprBuffer.append(e.toString()+e.getSuccCount()+"    ");
    	}
    	return exprBuffer.toString();
	}
}
