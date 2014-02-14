package diagnostic.explanation;

import graph.ConstraintPath;

import java.util.HashMap;
import java.util.Map;

import constraint.ast.Hypothesis;
import constraint.ast.Inequality;

public class HypothesisEntity extends Entity {
	private Inequality ieq;
	private Map<Hypothesis, Hypothesis> cachedEnv = new HashMap<Hypothesis, Hypothesis>();
	
	public HypothesisEntity(Inequality ieq) {
		super(0);
		this.ieq = ieq;
	}
	
	@Override
	public boolean explains(ConstraintPath p) {
		Inequality minHypo = p.getMinHypo();
		if (ieq.equals(minHypo))
			return true;

		Hypothesis env = p.getAssumption().addLeq(ieq.getFirstElement(), ieq.getSecondElement());
		
		if (cachedEnv.containsKey(env))
			env = cachedEnv.get(env);
		else {
			cachedEnv.put(env, env);
		}
		
		if (env.leq(minHypo.getFirstElement(), minHypo.getSecondElement()))
			return true;
			
		return false;
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof HypothesisEntity) {
			return ieq.equals(((HypothesisEntity) other).ieq);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return ieq.hashCode();
	}
	
	@Override
	public void toHTML(StringBuffer locBuf, StringBuffer exprBuf) {
		exprBuf.append(toString() + ";");
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer exprBuf) {
		exprBuf.append(toString() + ";");
	}
	
	@Override
	public String toString() {
		return ieq.toString();
	}
}
