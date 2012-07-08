package diagnositc;

import java.util.List;

import constraint.ast.Environment;
import constraint.ast.Constraint;

public class DiagnosisInput {
	Environment env;
	List<Constraint> constraints;
	
	public DiagnosisInput(Environment env, List<Constraint> cons) {
		this.env = env;
		constraints = cons;
	}
	
	public Environment getEnv() {
		return env;
	}
	
	public List<Constraint> getConstraints() {
		return constraints;
	}
}
