package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Variable extends Element {
	public Variable(String name, String info) {
		super(name, info);
	}
	
	public List<Variable> getVars () {
		List<Variable> ret = new ArrayList<Variable>();
		ret.add(this);
		return ret;
	}
	
	public boolean hasVars() {
		return true;
	}
	
	public boolean equals(Object o) {
		if (o instanceof Variable) {
			return this==o;
		}
		return false;
	}
}
