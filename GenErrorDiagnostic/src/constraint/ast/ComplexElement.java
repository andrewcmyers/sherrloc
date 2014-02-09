package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class ComplexElement extends EnumerableElement {
	Constructor cons;
	
	public ComplexElement(String name, Constructor cons, List<Element> elements) {
		super(name, elements);
		this.cons = cons;
	}
	
	public Constructor getCons() {
		return cons;
	}
	
	@Override
	String getSymbol() {
		return cons.toString();
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof ComplexElement) {
			ComplexElement ce = (ComplexElement) o;
			if (pos.equals(ce.pos) && cons.equals(ce.cons)) {
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
		int ret = cons.hashCode()*941;
		for (Element e : elements) {
			ret += e.hashCode()*13;
		}
		return ret+pos.hashCode();
	}
	
	@Override
	public boolean isTop() {
		for (Element e : elements) {
			if (!cons.contraVariant && !e.isTop())
				return false;
			if (cons.contraVariant && !e.isBottom())
				return false;
		}
		return true;
	}
	
	@Override
	public boolean isBottom() {
		for (Element e : elements) {
			if (!cons.contraVariant && !e.isBottom())
				return false;
			if (cons.contraVariant && !e.isTop())
				return false;
		}
		return true;
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
		return new ComplexElement(name, (Constructor)cons.getBaseElement(), baseElements) ;
	}
}
