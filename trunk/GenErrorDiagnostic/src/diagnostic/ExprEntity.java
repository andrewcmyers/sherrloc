package diagnostic;

import java.util.Map;

import constraint.ast.Element;
import constraint.graph.ConstraintPath;
import constraint.graph.ElementNode;
import constraint.graph.Node;

public class ExprEntity extends Entity {
	final private String expr;
	
	public ExprEntity(String expr, int succ) {
		super(succ);
		this.expr = expr;
	}
	
	@Override
	public boolean explains (ConstraintPath p) {
    	for (Node n : p.getAllNodes()) {
    		if (n.toString().equals(expr))
    			return true;
    	}
		return false;
	}
	
	@Override
	public void toHTML(Map<String,Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf) {
		Element en = ((ElementNode)exprMap.get(expr)).getElement();
		locBuf.append("['pathelement', \'"+en.getPosition()+"\'], ");
		exprBuf.append("<code>"+en.toSnippetString()+"</code>"+" [loc: "+en.getPosition()+"]    ");
	}
	
	@Override
	public void toConsole(Map<String, Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf) {
		Element en = ((ElementNode)exprMap.get(expr)).getElement();
		locBuf.append(en.getPosition() + ", ");
		exprBuf.append(en.toSnippetString() + ", ");
	}
	
	@Override
	public String toString() {
		return expr;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ExprEntity) {
			return expr.equals(((ExprEntity) obj).expr);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return expr.hashCode();
	}
}
