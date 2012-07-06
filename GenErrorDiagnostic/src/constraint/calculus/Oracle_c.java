package constraint.calculus;

import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;

/* 
 * a defalt oracle assumes an empty environment 
 * 
 */

public class Oracle_c implements Oracle {
	
	@Override
	public boolean leq(Environment env, Element e1, Element e2) {
		if (e1.equals(e2))
			return true;
		
		// e1 leq any element of e2
		if (e2 instanceof JoinElement) {
			for (Element e : ((JoinElement)e2).getElements())
				if (this.leq(env, e1, e)) 
					return true;
			return false;
		}
		// e1 leq all elements of e2
		else if (e2 instanceof MeetElement) {
			for (Element e : ((MeetElement)e2).getElements())
				if (this.leq(env, e1, e)) 
					return false;
			return true;
		}
		else
			return e1.leq_(e2);
	}
}
