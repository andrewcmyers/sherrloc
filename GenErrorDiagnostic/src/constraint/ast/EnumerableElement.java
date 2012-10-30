package constraint.ast;

import java.util.ArrayList;
import java.util.List;

/* a compond element is constructed by a Constructor */
public abstract class EnumerableElement extends Element {
	List<Element> elements;
	
	public EnumerableElement(String name, List<Element> elements) {
		super(name, Position.EmptyPosition());
		this.elements = elements;
	}
	
	public List<Element> getElements () {
		return elements;
	}
	
	// return the symbol of the current element, such as ->, *, meet, join
	abstract String getSymbol();
	
	public String toHTMLString() {
		if (!pos.isEmpty()) {
			return pos.snippet.toString();
		}
		String symbol = getSymbol();
		String ret = "";
		// infix
		if (symbol.equals("->") || symbol.equals("*") || symbol.equals("<-")) {
			return infixToHTMLString();
		}
		else {
			ret += symbol;
			for (Element e : elements)
				ret += " ("+e.toHTMLString() + ")";
		}
		return ret;
	}
	
	public String toDetailString() {
		String symbol = getSymbol();
		String ret = "";
		// infix
		if (symbol.equals("->") || symbol.equals("*") || symbol.equals("<-")) {
			return infixToHTMLString();
		}
		else {
			ret += symbol;
			for (Element e : elements)
				ret += " ("+e.toHTMLString() + ")";
		}
		return ret;
	}
	
	@Override
	public String toDotString() {
		String symbol = getSymbol();
		String ret = "";
		// infix
		if (symbol.equals("->") || symbol.equals("*")) {
			return infixToDotString();
		}
		else {
			ret += symbol;
			for (Element e : elements)
				ret += " ("+e.toDotString() + ")";
		}
		return ret;
	}
	
	public String infixToHTMLString () {
		String symbol = getSymbol();
		String ret = "";
		// infix
		ret += "("+elements.get(0).toHTMLString() + ")";
		for (int j=1; j<elements.size(); j++)
			ret += symbol + "(" + elements.get(j).toHTMLString() + ")";
		return ret;
	}
	
	public String infixToDotString () {
		String symbol = getSymbol();
		String ret = "";
		// infix
		ret += "("+elements.get(0).toDotString() + ")";
		for (int j=1; j<elements.size(); j++)
			ret += symbol + "(" + elements.get(j).toDotString() + ")";
		return ret;
	}
	
	public List<Variable> getVars () {
		List<Variable> ret = new ArrayList<Variable>();
		for (Element e : elements) {
			ret.addAll(e.getVars());
		}
		return ret;
	}
	
	public boolean hasVars() {
		for (Element e : elements)
			if (e.hasVars()) return true;
		return false;
	}
	
	public int hashCode() {
		int ret = 1;
		for (Element e : elements) {
			ret += e.hashCode();
		}
		return ret;
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof EnumerableElement) {
			EnumerableElement ce = (EnumerableElement)o;
			if (ce.getElements().size()==elements.size()) {
				for (int i=0; i<elements.size(); i++)
					if (!elements.get(i).equals(ce.getElements().get(i))) 
						return false;
				return true;
			}
		}
		return false;
	}
		
}
