package constraint.ast;

import java.util.ArrayList;
import java.util.List;

import util.StringUtil;

public class Variable extends Element {
	public Variable(String name, Position p) {
		super(name, p);
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
			return this.name==((Variable)o).name;
		}
		return false;
	}
		
	@Override
	public String toDetailString() {
		return toHTMLString();
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public String toDotString() {
		return StringUtil.getPrettyName(name);
	}
		
	@Override
	public boolean isStart() {
		return true;
	}
	
	@Override
	public boolean isEnd() {
		return true;
	}
	
	@Override
	public boolean isBottom() {
		return false;
	}
	
	@Override
	public boolean isTop() {
		return false;
	}
	
	@Override
	public Element getBaseElement() {
		return this;
	}
}
