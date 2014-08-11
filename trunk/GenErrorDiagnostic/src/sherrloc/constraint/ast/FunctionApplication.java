package sherrloc.constraint.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import sherrloc.graph.Variance;

/**
 * This class represents an application of a {@link Function}
 */
public class FunctionApplication extends Application {
	private final Function func;
	
	/**
	 * @param cons A constructor
	 * @param elements Parameters applied to the constructor <code>cons</code>
	 */
	public FunctionApplication(Function f, List<Element> elements) {
		super("", elements);
		this.func = f;
	}
	
	/**
	 * @return Function being applied to
	 */
	public Function getFunc() {
		return func;
	}
	
	@Override
	String getSymbol() {
		return func.toString();
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof FunctionApplication) {
			FunctionApplication fe = (FunctionApplication) o;
			if (pos.equals(fe.pos) && func.equals(fe.func)) {
				if (fe.getElements().size() == elements.size()) {
					for (int i = 0; i < elements.size(); i++) {
						if (!elements.get(i).equals(fe.getElements().get(i)))
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
		int ret = func.hashCode()*233;
		for (Element e : elements) {
			ret += e.hashCode()*17;
		}
		return ret+pos.hashCode();
	}
	
	@Override
	public boolean isTop() {
		return false;
	}
	
	@Override
	public boolean isBottom() {
		return false;
	}
	
	@Override
	public boolean trivialEnd() {
		return false;
	}
		
	@Override
	public Element getBaseElement() {
		List<Element> baseElements =  new ArrayList<Element>();
		for (Element e : elements) {
			baseElements.add(e.getBaseElement());
		}
		return new FunctionApplication((Function)func.getBaseElement(), baseElements) ;
	}
	
	@Override
	public Variance getVariance() {
		return Variance.NONE;
	}
	
	@Override
	public boolean unifyWith(Element e, Map<QuantifiedVariable, Element> map) {
		if (e instanceof FunctionApplication) {
			FunctionApplication fa = (FunctionApplication) e;
			if (!func.unifyWith(fa.func, map))
				return false;
			return super.unifyWith(e, map);
		}
		return false;
	}
	
	@Override
	public Element subst(Map<QuantifiedVariable, Element> map) {
		return new FunctionApplication(func, substElements(map));
	}
	
	@Override
	public boolean matches(Element e, Map<Variable, Element> map) {
		if (e instanceof FunctionApplication) {
			FunctionApplication fa = (FunctionApplication) e;
			if (!func.matches(fa.func, map))
				return false;
			return super.matches(e, map);
		}
		return false;
	}
	
	@Override
	public Application replace(Element e1, Element e2) {
		List<Element> eles =  new ArrayList<Element>();
		for (Element e : elements) {
			if (e.equals(e1))
				eles.add(e2);
			else if (e instanceof Application) {
				Application app = (Application) e;
				eles.add(app.replace(e1, e2));
			}
			else
				eles.add(e);
		}
		return new FunctionApplication(func, eles) ;
	}
}