package constraint.ast;

import java.util.ArrayList;
import java.util.List;

import javax.rmi.CORBA.Util;

import util.StringUtil;

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
			return this.name==((Variable)o).name;
		}
		return false;
	}
		
	@Override
	public String toDotString() {
		return StringUtil.getPrettyName(name);
	}
	
	@Override
	public boolean leq_(Object o, Environment env) {
		return false;
	}
	
	@Override
	public boolean isStart() {
		return false;
	}
	
	@Override
	public boolean isEnd() {
		return false;
	}
}
