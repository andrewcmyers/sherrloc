package diagnostic;

import java.util.Map;
import java.util.Set;

import util.HTTPUtil;
import constraint.ast.Element;
import constraint.graph.ElementNode;
import constraint.graph.Node;

public class ExprSuggestion implements Comparable<ExprSuggestion> {
	double weight = 0;
	Set<Entity> entities;
	double succCount;

	public ExprSuggestion(Set<Entity> exprs, double succCount, double weight) {
		this.entities = exprs;
		this.weight = weight;
		this.succCount = succCount;
	}
	
	public double getRank() {
		return weight;
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
		for (Entity e : entities) {
			Element en = ((ElementNode)exprMap.get(e.toString())).getElement();
    		locBuffer.append("['pathelement', \'"+en.getPosition()+"\'], ");
    		exprBuffer.append(en.toSnippetString()+" [loc: "+en.getPosition()+"]    ");
    	}
    	sb.append("<span class=\"path\" ");
		HTTPUtil.setShowHideActions(false, sb, locBuffer.toString(), 0);
		sb.append(">");
		sb.append("<code>"+exprBuffer.toString()+"</code></span>");
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
		for (Entity e : entities) {
			Element en = ((ElementNode)exprMap.get(e.toString())).getElement();
    		locBuffer.append(en.getPosition() + ", ");
    		exprBuffer.append(en.toSnippetString() + ", ");
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
