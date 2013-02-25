package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class MeetElement extends EnumerableElement {
	
	public MeetElement(String name, List<Element> elements) {
		super (name, elements);
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
	public boolean isStart() {
//		return !hasVars();
		return false;
	}
	
	@Override
	public boolean isEnd() {
		return false;
	}
	
	@Override
	public boolean equals (Object o) {
		if (this==o)
			return true;
		
		if (o instanceof MeetElement) {
			List<Element> list2 = ((MeetElement)o).elements;
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
	
	@Override
	public boolean isBottom() {
		for (Element e : elements) {
			if (e.isBottom())
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
