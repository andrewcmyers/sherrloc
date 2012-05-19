package constraint.ast;

import java.util.ArrayList;
import java.util.List;

/* a compond element is constructed by a Constructor */
public class CompondElement extends Element {
	Constructor cons;
	final List<Element> elements;
	
	public CompondElement(String name, Constructor cons, List<Element> elements) {
		super(name, "");
		this.cons = cons;
		this.elements = elements;
	}
	
	public Constructor getCons() {
		return cons;
	}
	
	public List<Element> getElements () {
		return elements;
	}
	
	public String toString() {
		String symbol = cons.toString();
		String ret = "";
		// infix
		if (symbol.equals("->") || symbol.equals("*")) {
			ret += "("+elements.get(0).toString() + ")";
			for (int j=1; j<elements.size(); j++)
				ret += symbol + "(" + elements.get(j).toString() + ")";
		}
		else {
			ret += symbol;
			for (Element e : elements)
				ret += " ("+e.toString() + ")";
		}
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
	
	public boolean equals(Object o) {
		if (o instanceof CompondElement) {
			CompondElement ce = (CompondElement)o;
			if (cons.equals(ce.cons) && ce.getElements().size()==elements.size()) {
				for (int i=0; i<elements.size(); i++)
					if (!elements.get(i).equals(ce.getElements().get(i))) 
						break;
				return true;
			}
		}
		return false;
	}
	
	public int hashCode() {
		int ret = cons.hashCode();
		for (Element e : elements) {
			ret ^= e.hashCode();
		}
		return ret;
	}
}
