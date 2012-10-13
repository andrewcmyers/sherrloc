package constraint.ast;

import java.util.List;

public class ConstructorElement extends EnumerableElement {
	Constructor cons;
	
	public ConstructorElement(String name, Constructor cons, List<Element> elements) {
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
		if (o instanceof ConstructorElement) {
			ConstructorElement ce = (ConstructorElement)o;
			if (cons.equals(ce.cons))
				return super.equals(o);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return cons.hashCode()^super.hashCode();
	}
	
	@Override
	public boolean isStart() {
		return true;
//		return !hasVars();
	}
	
	@Override
	public boolean isEnd() {
		return true;
//		return !hasVars();
	}
}
