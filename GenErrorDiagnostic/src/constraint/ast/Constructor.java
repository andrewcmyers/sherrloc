package constraint.ast;

import java.util.ArrayList;
import java.util.List;

public class Constructor extends Element {
	static int currentid=0;
	int		arity;
	boolean contraVariant;
	int id;
	
	public Constructor(String name, int arity, boolean contravariant) {
		super(name, Position.EmptyPosition());
		this.arity = arity;
		this.contraVariant = contravariant;
		id = currentid++;
	}
	
	private Constructor(String name, int arity, boolean contravariant, int id) {
		super(name, Position.EmptyPosition());
		this.arity = arity;
		this.contraVariant = contravariant;
		this.id = id;
	}
		
	public int getArity () {
		return arity;
	}
	
	public boolean isContraVariant() {
		return contraVariant;
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
			return arity==c.arity && this.name.equals(c.name) && this.id == c.id;
		}
		return false;
	}
		
	public boolean sameas (Object o) {
		if (o instanceof Constructor) {
			Constructor c = (Constructor)o;
			return arity==c.arity && this.name.equals(c.name);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return arity * 85751 + name.hashCode()*1913 + id;
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
		this.pos = pos;
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
	public Constructor getInstance () {
		if (arity==0)
			return new Constructor(name, arity, contraVariant);
		else
			return this;
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
		return new Constructor(name, arity, contraVariant, 0);
	}
}
