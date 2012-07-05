package constraint.ast;

import java.util.List;

public class MeetElement extends CompondElement {
	
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
		
		if (hasVars())
			return false;
		else 
			// TODO: correct?
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
