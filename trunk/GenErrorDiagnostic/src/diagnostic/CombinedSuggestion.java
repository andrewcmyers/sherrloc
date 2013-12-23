package diagnostic;

import java.util.Map;
import java.util.Set;

import util.HTTPUtil;
import constraint.ast.Element;
import constraint.graph.ElementNode;
import constraint.graph.Node;

public class CombinedSuggestion<K> implements Comparable<CombinedSuggestion<K>> {
	int rank = 0;
	Set<K> hypos;
	Set<String> exprs;
	Map<String, Double> succCount;

	public CombinedSuggestion(Set<K> hypos, Set<String> exprs, Map<String, Double> succCount) {
		this.exprs = exprs;
		this.hypos = hypos;
		rank = hypos.size()+exprs.size();
		this.succCount = succCount;
	}
	
	@Override
	public int compareTo(CombinedSuggestion<K> o) {
		return new Integer(rank).compareTo(o.rank);
	}
	
	public String toHTML (Map<String, Node> exprMap) {
		StringBuffer sb = new StringBuffer();
//		sb.append("<LI>\n");
		sb.append("<span class=\"rank\">(score "+rank+"("+hypos.size()+"+"+exprs.size()+"))</span> ");
		
		StringBuffer locBuffer = new StringBuffer();
    	StringBuffer exprBuffer = new StringBuffer();
		for (K hypo : hypos) {
			exprBuffer.append("# ");
			exprBuffer.append(hypo.toString());
			exprBuffer.append(" #");
    	}
    	for (String c : exprs) {
			Element en = ((ElementNode)exprMap.get(c)).getElement();
    		locBuffer.append("['pathelement', \'"+en.getPosition()+"\'], ");
    		exprBuffer.append(en.toSnippetString()+"    ");
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
