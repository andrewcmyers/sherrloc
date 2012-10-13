package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class JoinElement extends EnumerableElement {

	public JoinElement(String name, List<Element> elements) {
		super(name, elements);
		flat();
	}
	
	public String toHTMLString() {
		return infixToHTMLString();
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
			List<Element> list2 = ((JoinElement)o).elements;
			for (Element e : elements) {
				if (!list2.contains(e))
					return false;
			}
			for (Element e : list2) {
				if (!elements.contains(e))
					return false;
			}
			return true;
		}
		return false;
	}
		
}
