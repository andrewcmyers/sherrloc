package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class JoinElement extends EnumerableElement {

	public JoinElement(String name, List<Element> elements) {
		super(name, elements);
		flat();
	}
	
	public String toString( ) {
		return infixToString()+pos;
	}
	
	public String toSnippetString() {
		if (!pos.isEmpty()) {
			return pos.snippet.toString();
		}
		return infixToSnippetString();
	}
	
	@Override
	public String toDotString() {
		return infixToDotString();
	}
	
	public void flat () {
		List<Element> flat = new ArrayList<Element>();
		for (Element e : elements) {
			if (e instanceof JoinElement) {
				flat.addAll(((JoinElement) e).elements);
			}
			else
				flat.add(e);
		}
		elements = flat;
	}
	
	@Override
	String getSymbol() {
		return "join";
	}
	
	@Override
	public boolean isStart() {
		return false;
	}
	
	@Override
	public boolean isEnd() {
		return !this.hasVars();
	}

	@Override
	public boolean equals (Object o) {
		if (this == o)
			return true;
		
		if (o instanceof JoinElement) {
			return super.equals(o);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode()+9010;
	}
	
	@Override
	public boolean isTop() {
		for (Element e : elements) {
			if (e.isTop())
				return true;
		}
		return false;
	}
	
	@Override
	public boolean isBottom() {
		for (Element e : elements) {
			if (!e.isBottom())
				return false;
		}
		return true;
	}
	
	@Override
	public Element getBaseElement() {
		List<Element> baseElements =  new ArrayList<Element>();
		for (Element e : elements) {
			baseElements.add(e.getBaseElement());
		}
		return new JoinElement(name, baseElements);
	}
}
