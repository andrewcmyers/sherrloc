package constraint.ast;

import java.util.List;

public class JoinElement extends CompondElement {

	public JoinElement(String name, List<Element> elements) {
		super(name, elements);
	}
	
	public String toString() {
		return infixToString();
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
		
		if (hasVars())
			return false;
		else
			return false;
	}
	
	@Override
	public boolean leq_(Object o) {
		// check if all components are leq o
		for (Element e : elements) {
			if (! e.leq_(o))
				return false;
		}
		return true;
	}
}
