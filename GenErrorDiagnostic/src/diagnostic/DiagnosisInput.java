package diagnostic;

import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.Environment;

public class DiagnosisInput {
	Environment env;
	Set<Constraint> constraints;
	
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
