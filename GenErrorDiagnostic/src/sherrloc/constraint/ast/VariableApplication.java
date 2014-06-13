package sherrloc.constraint.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents an application of a {@link Variable}, possibly with
 * no parameters (e.g., x int, x)
 */
public class VariableApplication extends Application {
	private final Variable var;
	
	/**
	 * @param cons A constructor
	 * @param elements Parameters applied to the constructor <code>cons</code>
	 */
	public VariableApplication(Variable var, List<Element> elements) {
		super("", elements);
		this.var = var;
	}
	
	/**
	 * @return Variable being applied to
	 */
	public Variable getCons() {
		return var;
	}
	
	@Override
	String getSymbol() {
		return var.toString();
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof VariableApplication) {
			VariableApplication ce = (VariableApplication) o;
			if (pos.equals(ce.pos) && var.equals(ce.var)) {
				if (ce.getElements().size() == elements.size()) {
					for (int i = 0; i < elements.size(); i++) {
						if (!elements.get(i).equals(ce.getElements().get(i)))
							return false;
					}
					return true;
				}
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int ret = var.hashCode()*1579;
		for (Element e : elements) {
			ret += e.hashCode()*17;
		}
		return ret+pos.hashCode();
	}
	
	/**
	 * TODO: need to handle the variance of var
	 */
	@Override
	public boolean isTop() {
		return false;
	}
	
	/**
	 * Similar to {@link #isTop()}
	 */
	@Override
	public boolean isBottom() {
		return false;
	}
	
	@Override
	public boolean trivialEnd() {
		return true;
	}
		
	@Override
	public Element getBaseElement() {
		List<Element> baseElements =  new ArrayList<Element>();
		for (Element e : elements) {
			baseElements.add(e.getBaseElement());
		}
		return new VariableApplication((Variable)var.getBaseElement(), baseElements) ;
	}
	
	@Override
	public boolean isContraVariant() {
		return false;
	}
}
