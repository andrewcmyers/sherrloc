package diagnostic;

import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.Environment;

/**
 * Result of parser
 */
public class DiagnosisInput {
	private Environment env;
	private Set<Constraint> constraints;
	
	/**
	 * @param env Global assumptions
	 * @param cons A set of constraints
	 */
	public DiagnosisInput(Environment env, Set<Constraint> cons) {
		this.env = env;
		constraints = cons;
	}
	
	public Environment getEnv() {
		return env;
	}
	
	public Set<Constraint> getConstraints() {
		return constraints;
	}
}
