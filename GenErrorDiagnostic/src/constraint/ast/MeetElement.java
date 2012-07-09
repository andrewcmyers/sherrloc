package constraint.ast;

import java.util.List;

public class MeetElement extends EnumerableElement {
	
	public MeetElement(String name, List<Element> elements) {
		super (name, elements);
	}
	
	public String toString() {
		return infixToString();
	}
	
	@Override
	String getSymbol() {
		return "meet";
	}
	
	@Override
	public boolean isStart() {
		return !hasVars();
	}
	
	@Override
	public boolean isEnd() {
		return false;
	}
	
	@Override
	public boolean equals (Object o) {
		if (this==o)
			return true;
		
		if (o instanceof MeetElement && elements.size()==((MeetElement)o).elements.size()) {
			List<Element> list2 = ((MeetElement)o).elements;
			for (int i=0; i<elements.size(); i++) {
				if (!elements.get(i).equals(list2.get(i)))
					return false;
			}
			return true;
		}
		return false;
	}
	
	@Override
	public boolean leq_(Object o) {
		// check if any component is leq o
		for (Element e : elements) {
			if (e.leq_(o)) {
				return true;
			}
		}
		return false;
	}
}
