package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class JoinElement extends EnumerableElement {

	public JoinElement(String name, List<Element> elements) {
		super(name, elements);
		flat();
	}
	
	public String toString() {
		return infixToString();
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
		
		if (o instanceof JoinElement && elements.size()==((JoinElement)o).elements.size()) {
			List<Element> list2 = ((JoinElement)o).elements;
			for (int i=0; i<elements.size(); i++) {
				if (!elements.get(i).equals(list2.get(i)))
					return false;
			}
			return true;
		}
		return false;
	}
	
	@Override
	public boolean leq_(Object o, Environment env) {
		if (equals(o))
			return true;
		
		// check if all components are leq o
		for (Element e : elements) {
			if (! e.leq_(o, env))
				return false;
		}
		return true;
	}
}
