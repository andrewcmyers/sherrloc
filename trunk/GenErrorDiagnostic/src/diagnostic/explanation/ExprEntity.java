package diagnostic.explanation;

import graph.ConstraintPath;
import graph.Node;

public class ExprEntity extends Entity {
	final private String expr;
	final private String snippet;
	final private String loc;
	
	public ExprEntity(String snippet, String loc, int succ) {
		super(succ);
		this.snippet = snippet;
		this.expr = snippet+loc;
		this.loc = loc;
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
	public void toHTML(StringBuffer locBuf, StringBuffer exprBuf) {
		locBuf.append("['pathelement', \'"+loc+"\'], ");
		exprBuf.append("<code>"+snippet+"</code>"+" [loc: "+loc+"]    ");
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer exprBuf) {
		locBuf.append(loc + ", ");
		exprBuf.append(snippet+ ", ");
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
