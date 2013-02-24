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
		return this==o;
	}
	
	/* to make the diagnostic more precise, different instances of constants
	 * should be treated as separate nodes in the constraint flow graph
	 * However, when identifying unsat paths, the function sameas should be
	 * used 
	 */
	
	public boolean sameas (Object o) {
		if (o instanceof Constructor) {
			Constructor c = (Constructor)o;
			return arity==c.arity && this.name.equals(c.name);
		}
		return false;
	}
	
//	@Override
//	public int hashCode() {
//		return arity * 1000 + name.hashCode() + id;
//	}
		
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
}
