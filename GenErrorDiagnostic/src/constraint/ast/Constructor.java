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
		return toHTMLString()+pos.toString();
	}
		
	public String toHTMLString () {
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
	
	public String toDetailString () {
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
	 * However, when identifying unsat paths, the function sameas should be
	 * used 
	 * 
	 * The trick here is to generate an id number for each constructor. Constructors
	 * with different id numbers correspond to different nodes in graph.
	 * But any operation on partial orders should treat them as same element
	 */
	public Constructor getInstance ( ) {
		return new Constructor(name, arity, contraVariant, Position.EmptyPosition());
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
