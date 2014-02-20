package diagnostic.explanation;

import graph.ConstraintPath;
import graph.Edge;
import graph.ConstraintEdge;
import constraint.ast.Constraint;
import constraint.ast.Element;

/**
 * A basic unit of constraint explanation
 */
public class ConstraintEntity extends Entity {
	final private Constraint cons;
	
	/**
	 * @param cons A constraint
	 * @param succ # satisfiable paths using the constraint
	 */
	public ConstraintEntity(Constraint cons, int succ) {
		super(succ);
		this.cons = cons;
	}
	
	@Override
	public boolean explains(ConstraintPath p) {
		for (Edge edge : p.getEdges()) {
			if (edge instanceof ConstraintEdge && ((ConstraintEdge) edge).getConstraint().equals(cons)) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public void toHTML(StringBuffer locBuf, StringBuffer expBuf) {
		Element left = cons.getFirstElement();
		Element right = cons.getSecondElement();
		locBuf.append("['left', \'"+left.getPosition()+"\'], ");
		locBuf.append("['right', \'"+right.getPosition()+"\'], ");
		expBuf.append(cons.toHTMLString());
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer expBuf) {
		Element left = cons.getFirstElement();
		Element right = cons.getSecondElement();
		locBuf.append(left.getPosition() + ", ");
		locBuf.append(right.getPosition() + ", ");
		expBuf.append(cons.toConsoleString() + ", ");
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
