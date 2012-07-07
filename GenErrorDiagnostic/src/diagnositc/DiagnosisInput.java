package diagnositc;

import java.util.List;

import constraint.ast.Environment;
import constraint.ast.Equation;

public class DiagnosisInput {
	Environment env;
	List<Equation> constraints;
	
	public DiagnosisInput(Environment env, List<Equation> cons) {
		this.env = env;
		constraints = cons;
	}
	
	public Environment getEnv() {
		return env;
	}
	
	public List<Equation> getConstraints() {
		return constraints;
	}
}
