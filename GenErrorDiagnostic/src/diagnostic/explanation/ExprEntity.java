package diagnostic.explanation;

import graph.ConstraintPath;
import graph.Node;

public class ExprEntity extends Entity {
	final private String expr;
	final private String loc;
	
	public ExprEntity(String expr, String loc, int succ) {
		super(succ);
		this.expr = expr;
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
		exprBuf.append("<code>"+expr+"</code>"+" [loc: "+loc+"]    ");
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer exprBuf) {
		locBuf.append(loc + ", ");
		exprBuf.append(expr + ", ");
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
