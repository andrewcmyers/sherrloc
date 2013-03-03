package diagnostic;

import java.util.Map;
import java.util.Set;

import util.HTTPUtil;
import constraint.ast.Element;
import constraint.graph.ElementNode;
import constraint.graph.Node;

public class ExprSuggestion implements Comparable<ExprSuggestion> {
	int rank = 0;
	Set<String> exprs;
	int id;

	public ExprSuggestion(int id, Set<String> exprs, Map<String, Double> succCount) {
		this.exprs = exprs;
		this.id = id;
		for (String expr : exprs) {
			rank += succCount.get(expr);
		}
	}
	
	@Override
	public int compareTo(ExprSuggestion o) {
		return new Integer(rank).compareTo(o.rank);
	}
	
	public String toHTML (Map<String, Node> exprMap) {
		StringBuffer sb = new StringBuffer();
//		sb.append("<LI>\n");
		sb.append("<span class=\"rank\">(rank "+rank+")</span> ");
		
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
		for (String c : exprs) {
			Element en = ((ElementNode)exprMap.get(c)).getElement();
    		locBuffer.append("['pathelement', \'"+en.getPosition()+"\'], ");
    		exprBuffer.append(en.toHTMLString()+"    ");
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
}
