package diagnostic;

import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.Hypothesis;

/**
 * Result of parser
 */
public class DiagnosisInput {
	private Hypothesis env;
	private Set<Constraint> constraints;
	
	/**
	 * @param env Global assumptions
	 * @param cons A set of constraints
	 */
	public DiagnosisInput(Hypothesis env, Set<Constraint> cons) {
		this.env = env;
		constraints = cons;
	}
	
	public Hypothesis getEnv() {
		return env;
	}
	
	public Set<Constraint> getConstraints() {
		return constraints;
	}
}
