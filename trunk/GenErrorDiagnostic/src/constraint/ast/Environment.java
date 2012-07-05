package constraint.ast;

/**
 * Environment is a List of additional assumptions (in the form of equations)
 * That can be used in the leq check
 */
public class Environment {
	public boolean actsFor (String e1, String e2) {
		if (e2.equals("_"))
			return true;
		if (e1.equals("*"))
			return true;
		return e1.equals(e2);
	}
}
