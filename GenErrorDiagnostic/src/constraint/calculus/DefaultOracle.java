package constraint.calculus;

import constraint.ast.CompondElement;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;

/* 
 * a defalt oracle assumes an empty environment 
 * 
 */

public class DefaultOracle implements Oracle {
	
	@Override
	public boolean leq(Environment e, Element e1, Element e2) {
		if (e2 instanceof JoinElement && ((JoinElement)e2).getElements().contains(e1))
			return true;
		if (e1 instanceof MeetElement && ((MeetElement)e1).getElements().contains(e2))
			return true;
		return e1.leq(e2);
	}
}
