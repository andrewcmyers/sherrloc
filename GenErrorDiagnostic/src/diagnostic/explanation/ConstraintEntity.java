package diagnostic.explanation;

import graph.ConstraintPath;
import graph.Edge;
import graph.EquationEdge;
import constraint.ast.Constraint;
import constraint.ast.Element;

public class ConstraintEntity extends Entity {
	final private Constraint cons;
	
	public ConstraintEntity(Constraint cons, int succ) {
		super(succ);
		this.cons = cons;
	}
	
	@Override
	public boolean explains(ConstraintPath p) {
		for (Edge edge : p.getEdges()) {
			if (edge instanceof EquationEdge && ((EquationEdge) edge).getEquation().equals(cons)) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public void toHTML(StringBuffer locBuf, StringBuffer exprBuf) {
		Element left = cons.getFirstElement();
		Element right = cons.getSecondElement();
		locBuf.append("['left', \'"+left.getPosition()+"\'], ");
		locBuf.append("['right', \'"+right.getPosition()+"\'], ");
		exprBuf.append("<code>" + left.toString() + "</code>");
		exprBuf.append(" is less or equal than ");
		exprBuf.append("<code>" + right.toString() + "</code>");
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer exprBuf) {
		Element left = cons.getFirstElement();
		Element right = cons.getSecondElement();
		locBuf.append(left.getPosition() + ", ");
		locBuf.append(right.getPosition() + ", ");
		exprBuf.append(cons.toSnippetString() + ", ");
	}

	
	@Override
	public boolean equals(Object other) {
		if (other instanceof ConstraintEntity) {
			return cons.equals(((ConstraintEntity) other).cons);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return cons.hashCode();
	}
}
