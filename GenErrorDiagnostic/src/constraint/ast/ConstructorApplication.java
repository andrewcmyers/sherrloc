package constraint.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * This class represents an application of a {@link Constructor}, possibly with
 * no parameters (e.g., list int, int)
 */
public class ConstructorApplication extends EnumerableElement {
	private final Constructor cons;
	
	/**
	 * @param cons A constructor
	 * @param elements Parameters applied to the constructor <code>cons</code>
	 */
	public ConstructorApplication(Constructor cons, List<Element> elements) {
		super("", elements);
		this.cons = cons;
	}
	
	/**
	 * @return Constructor being applied to
	 */
	public Constructor getCons() {
		return cons;
	}
	
	@Override
	String getSymbol() {
		return cons.toString();
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof ConstructorApplication) {
			ConstructorApplication ce = (ConstructorApplication) o;
			if (pos.equals(ce.pos) && cons.equals(ce.cons)) {
				if (ce.getElements().size() == elements.size()) {
					for (int i = 0; i < elements.size(); i++) {
						if (!elements.get(i).equals(ce.getElements().get(i)))
							return false;
					}
					return true;
				}
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int ret = cons.hashCode()*941;
		for (Element e : elements) {
			ret += e.hashCode()*13;
		}
		return ret+pos.hashCode();
	}
	
	/**
	 * Special handling is need to lift the "topness". For example, in Jif,
	 * Top->Top, where Top is the top element in the lattice of principals, is
	 * the top element in the lattice of labels
	 */
	@Override
	public boolean isTop() {
		for (Element e : elements) {
			if (!cons.isContraVariant() && !e.isTop())
				return false;
			if (cons.isContraVariant() && !e.isBottom())
				return false;
		}
		return true;
	}
	
	/**
	 * Similar to {@link #isTop()}
	 */
	@Override
	public boolean isBottom() {
		for (Element e : elements) {
			if (!cons.isContraVariant() && !e.isBottom())
				return false;
			if (cons.isContraVariant() && !e.isTop())
				return false;
		}
		return true;
	}
	
	@Override
	public boolean trivialEnd() {
		return false;
	}
		
	@Override
	public Element getBaseElement() {
		List<Element> baseElements =  new ArrayList<Element>();
		for (Element e : elements) {
			baseElements.add(e.getBaseElement());
		}
		return new ConstructorApplication((Constructor)cons.getBaseElement(), baseElements) ;
	}
	
	public static void main(String[] args) {
		Variable a = new Variable("x", Position.EmptyPosition());
		Variable b = new Variable("y", Position.EmptyPosition());
		List<Element> l = new ArrayList<Element>();
		l.add(a);
		l.add(b);
		ConstructorApplication ca1 = new ConstructorApplication(new Constructor("arrow", 2, false, Position.EmptyPosition()), l);
		Element e1 = ca1.getInstance();
		e1.setPosition(new Position("aa", "aa", 10, 10, 10, 12));
		ConstructorApplication ca2 = new ConstructorApplication(new Constructor("arrow", 2, false, Position.EmptyPosition()), l);
		Element e2 = ca2.getInstance();
		e2.setPosition(new Position("aa", "aa", 10, 10, 10, 14));
		System.out.println(e1.equals(e2));
		System.out.println(e1.hashCode() == e2.hashCode());
	}
}
