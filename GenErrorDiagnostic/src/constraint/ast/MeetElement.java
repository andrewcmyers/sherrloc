package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class MeetElement extends EnumerableElement {
	
	public MeetElement(String name, List<Element> elements) {
		super (name, elements);
		flat();
	}
	
	public String toString( ) {
		return infixToString();
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
			if (e instanceof MeetElement) {
				flat.addAll(((MeetElement) e).elements);
			}
			else
				flat.add(e);
		}
		elements = flat;
	}
	
	@Override
	String getSymbol() {
		return "meet";
	}
		
	@Override
	public boolean equals (Object o) {
		if (this==o)
			return true;
		
		if (o instanceof MeetElement) {
			return super.equals(o);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode()+2434;
	}
	
	@Override
	public boolean isTop() {
		for (Element e : elements) {
			if (!e.isTop())
				return false;
		}
		return true;
	}
	
	@Override
	public boolean isBottom() {
		for (Element e : elements) {
			if (e.isBottom())
				return true;
		}
		return false;
	}	
	
	@Override
	public boolean trivialEnd() {
		for (Element e : elements) {
			if (e.trivialEnd())
				return true;
		}
		return false;
	}
		
	@Override
	public Element getBaseElement() {
		List<Element> baseElements =  new ArrayList<Element>();
		for (Element e : elements) {
			baseElements.add(e.getBaseElement());
		}
		return new MeetElement(name, baseElements);
	}
}
