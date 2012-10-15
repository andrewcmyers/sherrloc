package constraint.ast;

import java.util.List;

public class Constraint implements Comparable<Constraint> {	
	Element e1, e2;
	Relation r;
	Position pos;
	int succPaths=0;
	int cause = 0;
	Environment assumption;
	
	public Constraint(Element e1, Element e2, Relation r, Environment assumption, Position pos) {
		this.e1 = e1;
		this.e2 = e2;
		this.r = r;
		this.assumption = assumption;
		this.pos = pos;
	}
	
	public Element getFirstElement () {
		return e1;
	}
	
	public Element getSecondElement () {
		return e2;
	}
	
	public Relation getRelation () {
		return r;
	}
	
	public Environment getAssumption() {
		return assumption;
	}
	
	public String toString () {
		return e1.toString()+r.toString()+e2.toString()+pos.toString();
	}
	
	public String toDotString () {
		return e1.toDotString()+r.toString()+e2.toDotString();
	}
	
	public String toHTMLString () {
		return e1.toHTMLString()+r.toString()+e2.toHTMLString();
	}
	
	public List<Variable> getVars () {
		List<Variable> ret = e1.getVars();
		ret.addAll(e2.getVars());
		return ret;
	}
	
	public void incSuccPaths () {
		System.out.println("succpath increased "+toString());
		succPaths ++;
	}
	
	public int getSuccPaths() {
		return succPaths;
	}
	
	public void setCause () {
		cause ++;
	}
	
	public double getRank () {
    	return succPaths;
//        return ((double)count)/((double)totalcount);
    }
	
	public Position getPos() {
		return pos;
	}
    
    public int compareTo(Constraint n) {
        double rank1 = getRank();
        double rank2 = n.getRank();
        return Double.compare(rank2, rank1);
    }
    
    @Override
    public boolean equals(Object obj) {
    	if (obj instanceof Constraint) {
    		return this.toString().equals(((Constraint)obj).toString());
    	}
    	return false;
    }
    
    @Override
    public int hashCode() {
    	return toString().hashCode();
    }
}
