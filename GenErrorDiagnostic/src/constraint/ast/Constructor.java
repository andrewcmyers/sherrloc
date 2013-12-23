package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Constructor extends Element {
	int		arity;
	boolean contraVariant;
	
	public Constructor(String name, int arity, boolean contravariant, Position p) {
		super(name, p);
		this.arity = arity;
		this.contraVariant = contravariant;
	}
	
	public int getArity () {
		return arity;
	}
	
	public boolean isContraVariant() {
		return contraVariant;
	}
	
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
	
	public String toDotString () {
		return toString();
	}
		
	public List<Variable> getVars () {
		return new ArrayList<Variable>();
	}
	
	public boolean hasVars() {
		return false;
	}
	
	@Override
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
	public boolean isStart() {
		return true;
	}
	
	@Override
	public boolean isEnd() {
		return true;
	}
	
	/* to make the diagnostic more precise, different instances of constants
	 * should be treated as separate nodes in the constraint flow graph
	 * However, when identifying unsat paths, these constructors are the same. 
	 * 
	 * The trick here is to generate new instances of same constructor, but
	 * But any operation on partial orders should call getBaseElement() in order
	 * to treat the duplicated nodes in the same way
	 */
	public Constructor getInstance ( ) {
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
	public Element getBaseElement() {
		return new Constructor(name, arity, contraVariant, Position.EmptyPosition());
	}	
}
