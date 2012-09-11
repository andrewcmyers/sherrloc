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
	
	public String toDotString () {
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
			return arity==c.arity && this.name.equals(c.name);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return arity * 1000 + name.hashCode();
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
