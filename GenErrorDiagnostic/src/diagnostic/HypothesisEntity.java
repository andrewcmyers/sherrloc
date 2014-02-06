package diagnostic;

import java.util.Map;

import constraint.ast.Environment;
import constraint.ast.Hypothesis;
import constraint.graph.ConstraintPath;
import constraint.graph.Node;

public class HypothesisEntity extends Entity {
	private Hypothesis hypo;
	private Map<Environment, Environment> cachedEnv;
	
	public HypothesisEntity(Hypothesis hypo, Map<Environment, Environment> cachedEnv) {
		super(0);
		this.hypo = hypo;
		this.cachedEnv = cachedEnv;
	}
	
	@Override
	public boolean explains(ConstraintPath p) {
		Hypothesis minHypo = p.getMinHypo();
		if (hypo.equals(minHypo))
			return true;

		Environment env = p.getAssumption().addLeq(hypo.getFirst(), hypo.getSecond());
		
		if (cachedEnv.containsKey(env))
			env = cachedEnv.get(env);
		else {
			cachedEnv.put(env, env);
		}
		
		if (env.leq(minHypo.getFirst(), minHypo.getSecond()))
			return true;
			
		return false;
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof HypothesisEntity) {
			return hypo.equals(((HypothesisEntity) other).hypo);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return hypo.hashCode();
	}
	
	@Override
	public void toHTML(Map<String, Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf) {
		exprBuf.append(toString() + ";");
	}
	
	@Override
	public void toConsole(Map<String, Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf) {
		
	}
	
	@Override
	public String toString() {
		return hypo.toString();
	}
}
