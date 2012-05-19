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
	
	public List<Variable> getVars () {
		return new ArrayList<Variable>();
	}
	
	public boolean hasVars() {
		return false;
	}
	
	public boolean equals(Object o) {
		if (o instanceof Constructor) {
			return this==o;
		}
		return false;
	}
	
}
