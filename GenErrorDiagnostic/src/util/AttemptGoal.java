package util;

import constraint.ast.Environment;
import constraint.graph.ElementNode;

public class AttemptGoal {

	private final ElementNode n1;
	private final ElementNode n2;
	private final Environment env;

	public AttemptGoal (ElementNode n1, ElementNode n2, Environment env) {
		this.n1 = n1;
		this.n2 = n2;
		this.env = env;
	}

	public ElementNode getSource() {
		return n1;
	}

	public ElementNode getSink() {
		return n2;
	}
	
	public Environment getEnv() {
		return env;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof AttemptGoal) {
			return ((AttemptGoal) obj).n1.equals(n1) && ((AttemptGoal) obj).n2.equals(n2);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return n1.hashCode() + n2.hashCode() + env.hashCode();
	}
}
