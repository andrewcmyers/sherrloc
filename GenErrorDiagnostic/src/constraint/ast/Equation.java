package constraint.ast;

import java.util.List;

public class Equation {	
	Element e1, e2;
	Relation r;
	String info;
	
	public Equation(Element e1, Element e2, Relation r, String info) {
		this.e1 = e1;
		this.e2 = e2;
		this.r = r;
		this.info = info;
	}
	
	public Element getFirstElement () {
		return e1;
	}
	
	public Element getSecondElement () {
		return e2;
	}
	
	public Relation getRelation () {
		return r;
	}
	
	public String toString () {
		String pos = info.equals("")?"":(" @"+info);
		return e1.toString()+r.toString()+e2.toString()+pos;
	}
	
	public String toDotString () {
		return e1.toString()+r.toString()+e2.toString();
	}
	
	public List<Variable> getVars () {
		List<Variable> ret = e1.getVars();
		ret.addAll(e2.getVars());
		return ret;
	}	
	
}
