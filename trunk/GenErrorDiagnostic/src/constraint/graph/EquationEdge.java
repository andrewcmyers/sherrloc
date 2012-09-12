package constraint.graph;

import constraint.ast.Constraint;
import constraint.ast.Environment;
import constraint.ast.Relation;

/* 
 * an edge in graph is either static (join, meet) or dynamic (a flow that generated from an equation) 
 * the difference is determined by if equ is null for now
 * Do we really need static links? Yes, that's a special case of constructor
 */ 
public class EquationEdge extends Edge {
    Constraint equ;
    
    public EquationEdge(Constraint e, Node from, Node to) {
    	super(from, to);
        this.equ = e;
    }
    
    public int getLineno() { 
//        if (cons != null && cons.position()!= null)
//            return cons.position().line();
//        else return 0;
        return 0;
    }       

    public Constraint getEquation() {
		return equ;
	}
    
    @Override
    public String toString() {
        return equ.toString(); 
    }
    
//    public String toStringDetail() {
//        if (equ != null) {
//            return toString() + "\n(Why this constraint?) " + equ.detailMsg();
//        }
//        else return toString();
//    }
    @Override
    public boolean equals(Object obj) {
    	if (obj instanceof EquationEdge)
    		return equ.equals(((EquationEdge) obj).getEquation());
    	return false;
    }
    
    @Override
    public int hashCode() {
    	return equ.hashCode();
    }
    
    @Override
    public String toDotString() {
        return  equ.toDotString(); // +"\n"+ cons.detailMsg(); 
    }
    
    @Override
    public Environment getAssumption() {
    	return equ.getAssumption();
    }
    
    @Override
    public boolean isDirected() {
    	return equ.getRelation()!=Relation.EQ;
    }
    
    @Override
    public void setCause() {
    	equ.setCause();
    }
}
