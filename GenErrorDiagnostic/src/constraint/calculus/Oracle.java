package constraint.calculus;

import constraint.ast.Element;
import constraint.ast.Environment;

public interface Oracle {
	boolean leq (Environment e, Element e1, Element e2);
}
