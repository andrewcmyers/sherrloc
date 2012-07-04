package constraint.ast;

import java.util.List;

public class MeetElement extends CompondElement {
	
	public MeetElement(String name, Constructor cons, List<Element> elements) {
		super (name, cons, elements);
	}
	
	public String toString() {
		return infixToString();
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
}
