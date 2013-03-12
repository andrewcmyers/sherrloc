package constraint.ast;


/* a hypothesis is simply a partial order on two elements */

public class Hypothesis {
	private final Element e1;
	private final Element e2;

	public Hypothesis (Element e1, Element e2) {
		this.e1 = e1.getBaseElement();
		this.e2 = e2.getBaseElement();
	}

	public Element getFirst() {
		return e1;
	}

	public Element getSecond() {
		return e2;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Hypothesis) {
			Hypothesis g = (Hypothesis) obj;
			return g.e1.equals(e1) && g.e2.equals(e2);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return e1.hashCode() * 70691 + e2.hashCode();
	}
	
	@Override
	public String toString() {
		return e1 + " <= " + e2;
	}
}
