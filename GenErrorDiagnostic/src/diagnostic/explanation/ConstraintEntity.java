package diagnostic.explanation;

import graph.ConstraintEdge;
import graph.ConstraintPath;
import graph.Edge;
import constraint.ast.Constraint;

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
			if (edge instanceof ConstraintEdge && ((ConstraintEdge) edge).getConstraint().toString().equals(cons.toString())) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public void toHTML(StringBuffer locBuf, StringBuffer expBuf) {
		locBuf.append("['left', \'"+cons.getPos()+"\'], ");
		expBuf.append(cons.toHTMLString());
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer expBuf) {
		locBuf.append(cons.getPos() + ", ");
		expBuf.append(cons.toConsoleString() + ", ");
	}

	
	@Override
	public boolean equals(Object other) {
		if (other instanceof ConstraintEntity) {
			return cons.toString().equals(((ConstraintEntity) other).cons.toString());
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return cons.toString().hashCode();
	}
}
