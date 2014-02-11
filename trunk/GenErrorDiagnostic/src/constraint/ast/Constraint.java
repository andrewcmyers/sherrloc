package constraint.ast;

import java.util.List;

import util.PrettyPrinter;

/**
 * A constraint has the format of {@link Environment} entails {@link Relation}
 * (e1 , e2), where {@link Relation} can be <= or ==
 * 
 */
public class Constraint implements Comparable<Constraint>, PrettyPrinter {	
	private final Element e1, e2;
	private final Relation r;
	private final Position pos;
	private final Environment assumption;

	private int succPaths=0;
	
	/**
	 * Creates a constraint <code>assumption</code> entails
	 * <code>r(e1,e2)</code>. Position information is also required to trace the
	 * error cause back into program
	 * 
	 * @param e1 An element to be constrained
	 * @param e2 An element to be constrained
	 * @param r A relation on <code>e1, e2</code>
	 * @param assumption Hypothesis of the generated constraint
	 * @param pos Source program position that generates the constraint
	 */
	public Constraint(Element e1, Element e2, Relation r, Environment assumption, Position pos) {
		this.e1 = e1;
		this.e2 = e2;
		this.r = r;
		this.assumption = assumption;
		this.pos = pos;
	}
	
	/**
	 * @return The constraint where all elements has an empty position. See
	 *         {@link Element#getBaseElement()}
	 */
	public Constraint baseConstraint () {
		return new Constraint(e1.getBaseElement(), e2.getBaseElement(), r, assumption, pos); 
	}
	
	/**
	 * @return Element <code>e1</code> in constraint <code>assumption</code>
	 *         entails <code>r(e1,e2)</code>
	 */
	public Element getFirstElement () {
		return e1;
	}

	/**
	 * @return Element <code>e2</code> in constraint <code>assumption</code>
	 *         entails <code>r(e1,e2)</code>
	 */
	public Element getSecondElement () {
		return e2;
	}
	
	/**
	 * @return Relation (<= or ==)
	 */
	public Relation getRelation () {
		return r;
	}
	
	/**
	 * 
	 * @return Hypothesis <code>assumption</code> in constraint
	 *         <code>assumption</code> entails <code>r(e1,e2)</code>
	 */
	public Environment getAssumption() {
		return assumption;
	}
	
	@Override
	public String toString () {
		return e1.toString()+r.toString()+e2.toString()+pos.toString();
	}
	
	@Override
	public String toDotString () {
		return e1.toDotString()+r.toString()+e2.toDotString();
	}
	
	@Override
	public String toHTMLString() {
		return ("<code>" + e1.toString()
				+ "</code> is less or equal than <code>" + e2.toString() + "</code>");
	}
	
	@Override
	public String toConsoleString() {
		return e1.toSnippetString()+r.toString()+e2.toSnippetString();
	}
	
	/**
	 * @return All variables used in the constraint
	 */
	public List<Variable> getVars () {
		List<Variable> ret = e1.getVars();
		ret.addAll(e2.getVars());
		return ret;
	}
	
	/**
	 * Increase # satisfiable paths using this constraint
	 */
	public void incNumSuccPaths () {
		succPaths ++;
	}
	
	/**
	 * @return # satisfiable paths using this constraint
	 */
	public int getNumSuccPaths () {
    	return succPaths;
    }
	
	/**
	 * @return Source code position that generates the constraint
	 */
	public Position getPos() {
		return pos;
	}
    
	@Override
    public int compareTo(Constraint n) {
        double rank1 = getNumSuccPaths();
        double rank2 = n.getNumSuccPaths();
        return Double.compare(rank2, rank1);
    }
    
    @Override
    public boolean equals(Object obj) {
    	if (obj instanceof Constraint) {
    		Constraint c = (Constraint) obj;
    		return e1.equals(c.e1) && e2.equals(c.e2) 
    		&& r.equals(c.r) && pos.equals(c.pos);
    	}
    	return false;
    }
    
    @Override
    public int hashCode() {
    	return e1.hashCode()*4759 + e2.hashCode()*523 + pos.hashCode()*3 + ((r==Relation.EQ)?1:0);
    }
}
