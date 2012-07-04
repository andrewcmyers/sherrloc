package constraint.ast;

import java.util.List;

public class JoinElement extends CompondElement {

	public JoinElement(String name, Constructor cons, List<Element> elements) {
		super(name, cons, elements);
	}
	
	public String toString() {
		return infixToString();
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
		
		if (hasVars())
			return false;
		else
			return false;
	}
}
