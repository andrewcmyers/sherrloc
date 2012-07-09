package constraint.ast;

import java.util.HashSet;
import java.util.Set;

public class DisjunctivePrincipal extends Principal {
	Set<BasicPrincipal> set;
	
	public DisjunctivePrincipal(String s) {
		super(s);
		set = new HashSet<BasicPrincipal>();
		
		String[] comp = s.split(",");
		for (String c : comp) {
			set.add(new BasicPrincipal(c));
		}
	}
	
    public Set<BasicPrincipal> disjuncts() {
    	return set;
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
    public boolean actsFor(BasicPrincipal p) {
        // dp actsfor granter if all of the disjucts act for granter
        boolean all = true;
        for (BasicPrincipal bp : set) {
            if (!bp.actsFor(p))
            	return false;
        }
        return true;
    }
}
