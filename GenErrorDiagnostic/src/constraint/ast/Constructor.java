package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Constructor extends Element {
	int		arity;
	
	public Constructor(String name, int arity) {
		super(name, "");
		this.arity = arity;
	}
	
	public int getArity () {
		return arity;
	}
	
	/**
	 * To simplify the output, use UpperCase to indicate a constructor
	 */
	public String toString () {
		if (name.equals("arrow"))
			return "->";
		else if (name.equals("pair"))
			return "*";
		else
			return name.toUpperCase();
	}
	
	@Override
	public String toDotString() {
		return toString();
	}
	
	public List<Variable> getVars () {
		return new ArrayList<Variable>();
	}
	
	public boolean hasVars() {
		return false;
	}
	
	public boolean equals(Object o) {
		if (o instanceof Constructor) {
			Constructor c = (Constructor)o;
			return arity==c.arity && this.name==c.name;
		}
		return false;
	}
	
	@Override
	public boolean leq_(Object o, Environment env) {
		return equals(o);
	}
	
	@Override
	public boolean isStart() {
		return true;
	}
	
	@Override
	public boolean isEnd() {
		return true;
	}
}
