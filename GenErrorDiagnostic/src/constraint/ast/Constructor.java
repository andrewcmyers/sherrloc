package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Constructor extends Element {
	int		arity;
	boolean contraVariant;
	
	/**
	 * @param name Constructor name
	 * @param arity Arity of the constructor
	 * @param contravariant False if all parameters are covariant; true if all parameters are contravariant
	 * @param p Position of the element in source code
	 */
	public Constructor(String name, int arity, boolean contravariant, Position p) {
		super(name, p);
		this.arity = arity;
		this.contraVariant = contravariant;
	}
	
	/**
	 * @return Arity of the constructor
	 */
	public int getArity () {
		return arity;
	}
	
	/**
	 * @return False if all parameters are covariant; true if all parameters are contravariant
	 */
	public boolean isContraVariant() {
		return contraVariant;
	}
	
	@Override
	public String toString () {
		if (name.equals("arrow"))
			return "->";
		if (name.equals("larrow"))
			return "<-";
		else if (name.equals("pair"))
			return "*";
		else
			return name;
	}
			
	@Override
	public String toSnippetString () {
		if (!pos.isEmpty())
			return pos.snippet;
		
		if (name.equals("arrow"))
			return "->";
		if (name.equals("larrow"))
			return "<-";
		else if (name.equals("pair"))
			return "*";
		else
			return name;
	}
	
	@Override
	public String toDotString () {
		return toString();
	}
		
	@Override
	public List<Variable> getVars () {
		return new ArrayList<Variable>();
	}
	
	@Override
	public boolean hasVars() {
		return false;
	}
	
	@Override
	/**
	 * Same constructor as different positions are treated as different elements to improve the precision of error diagnosis
	 */
	public boolean equals(Object o) {
		if (o instanceof Constructor) {
			Constructor c = (Constructor)o;
			return arity==c.arity && this.name.equals(c.name) && this.contraVariant==c.contraVariant && this.pos.equals(c.pos);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return arity * 85751 + name.hashCode()*1913 + pos.hashCode()*3 + (this.contraVariant?1:0);
	}
		
	@Override
	public Constructor clone ( ) {
		return new Constructor(name, arity, contraVariant, pos);
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
	public boolean trivialEnd() {
		return false;
	}
	
	@Override
	public Element getBaseElement() {
		return new Constructor(name, arity, contraVariant, Position.EmptyPosition());
	}	
}
