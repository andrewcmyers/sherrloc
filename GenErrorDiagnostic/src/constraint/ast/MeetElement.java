package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class MeetElement extends EnumerableElement {
	
	public MeetElement(String name, List<Element> elements) {
		super (name, elements);
		flat();
	}
	
	public String toString() {
		return infixToString();
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
		return false;
//		return !hasVars();
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
	public boolean isDecomposable() {
		return false;
	}
}
