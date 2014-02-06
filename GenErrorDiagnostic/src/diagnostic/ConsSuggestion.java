package diagnostic;

import graph.EquationEdge;

import java.util.Set;

import util.HTTPUtil;
import constraint.ast.Element;

public class ConsSuggestion implements Comparable<ConsSuggestion> {
	int rank = 0;
	Set<EquationEdge> edges;
	int id;

	public ConsSuggestion(int id, Set<EquationEdge> edges) {
		this.edges = edges;
		this.id = id;
		for (EquationEdge edge : edges) {
			rank += edge.getEquation().getSuccPaths();
		}
	}

	@Override
	public int compareTo(ConsSuggestion o) {
		return new Integer(rank).compareTo(o.rank);
	}

	public String toHTML() {
		StringBuffer sb = new StringBuffer();
		sb.append("<span class=rank>(rank " + rank + ")</span> ");
		for (EquationEdge c : edges) {
			StringBuffer locBuffer = new StringBuffer();
			Element left = c.getEquation().getFirstElement();
			locBuffer.append("['left', \'" + left.getPosition().toString()
					+ "\'],");
			Element right = c.getEquation().getSecondElement();
			locBuffer.append("['right', \'" + right.getPosition().toString()
					+ "\']");
			String loc = locBuffer.toString();
			sb.append("<span class=\"mincut\" ");
			HTTPUtil.setShowHideActions(true, sb, loc, id);
			sb.append(">");
			sb.append("<code id=\"left" + id + "\">" + left.toString()
					+ "</code>");
			sb.append(" has the same type as ");
			sb.append("<code id=\"right" + id + "\">" + right.toString()
					+ "</code></span>");
			sb
					.append("<button onclick=\"hide_all();show_elements_perm(false, [");
			sb.append(loc);
			sb.append("]); ");
			sb.append("show_cut_perm(" + id + ")\" ");
			// setShowHideActions(false, sb, loc, id);
			sb.append(">show it</button><br>\n");
		}
		return sb.toString();
	}
}
