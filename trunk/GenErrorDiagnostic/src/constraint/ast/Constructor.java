package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Constructor extends Element {
	int		arity;
	boolean contraVariant;
	
	public Constructor(String name, int arity, boolean contravariant) {
		super(name, Position.EmptyPosition());
		this.arity = arity;
		this.contraVariant = contravariant;
	}
	
	public int getArity () {
		return arity;
	}
	
	public boolean isContraVariant() {
		return contraVariant;
	}
		
	/**
	 * To simplify the output, use UpperCase to indicate a constructor
	 */
	public String toHTMLString () {
		if (name.equals("arrow"))
			return "->";
		if (name.equals("larrow"))
			return "<-";
		else if (name.equals("pair"))
			return "*";
		else
			return name;
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
	
	@Override
	public void setPosition(Position pos) {
		// do nothing, since it does not make any sense 
	}
	
}
