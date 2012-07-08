package constraint.graph;

import constraint.ast.Constraint;

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
    public String toDotString() {
        return  equ.toDotString(); // +"\n"+ cons.detailMsg(); 
    }
    
//    public LabelEnv getEnv () {
//        if (cons!=null)
//            return cons.env();
//        else
//            return null;
//    }
    
    @Override
    public boolean isDirected() {
    	return false;
    }
    
    @Override
    public void setCause() {
    	equ.setCause();
    }
}
