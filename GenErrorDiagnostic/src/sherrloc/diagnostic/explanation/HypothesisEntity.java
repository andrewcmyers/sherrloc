package sherrloc.diagnostic.explanation;

import java.util.Map;

import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.ast.Inequality;
import sherrloc.graph.ConstraintPath;

/**
 * A basic unit of missing hypothesis explanation
 */
public class HypothesisEntity extends Entity {
	final private Inequality ieq;
	private Map<Hypothesis, Hypothesis> cachedEnv;	
	
	/**
	 * @param ieq An inequality
	 * @param cachedEnv: reuse saturated hypothesis graph for better performance
	 */
	public HypothesisEntity(Inequality ieq, Map<Hypothesis, Hypothesis> cachedEnv) {
		super(0);
		this.ieq = ieq;
		this.cachedEnv = cachedEnv;
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
		exprBuf.append(toString());
	}
	
	@Override
	public void toConsole(StringBuffer locBuf, StringBuffer exprBuf) {
		exprBuf.append(toString());
	}
	
	@Override
	public String toString() {
		return ieq.toString();
	}
}
