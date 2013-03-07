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
		return cons.toHTMLString();
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof ComplexElement) {
			ComplexElement ce = (ComplexElement)o;
			if (cons.equals(ce.cons))
				return super.equals(o);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return cons.hashCode()*31+super.hashCode();
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
	public boolean isStart() {
		return true;
	}
	
	@Override
	public boolean isEnd() {
		return true;
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
