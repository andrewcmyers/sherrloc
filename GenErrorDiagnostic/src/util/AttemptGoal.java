package util;

import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.ast.Hypothesis;

public class AttemptGoal {

	private final Element e1;
	private final Element e2;
	private final Environment env;

	public AttemptGoal (Element e1, Element e2, Environment env) {
		this.e1 = e1.getBaseElement();
		this.e2 = e2.getBaseElement();
		this.env = env;
	}

	public Element getSource() {
		return e1;
	}

	public Element getSink() {
		return e2;
	}
	
	public Environment getEnv() {
		return env;
	}
	
	public Hypothesis getSufficientHypo () {
		return new Hypothesis(e1, e2);
	}
	
//	@Override
//	public boolean equals(Object obj) {
//		if (obj instanceof AttemptGoal) {
//			AttemptGoal g = (AttemptGoal) obj;
//			return g.e1.equals(e1) && g.e2.equals(e2) && g.env.equals(env);
//		}
//		return false;
//	}
//	
//	@Override
//	public int hashCode() {
//		return e1.hashCode() + e2.hashCode() + env.hashCode();
//	}
	
	@Override
	public String toString() {
		return e1 + " <= " + e2;
	}
}
