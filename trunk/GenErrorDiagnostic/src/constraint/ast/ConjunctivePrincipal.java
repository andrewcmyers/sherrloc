package constraint.ast;

import java.util.HashSet;
import java.util.Set;

/** 
 * A conjunctive principal represents the conjunction of two principals A&B.
 * The conjunctive principal A&B can act for A and it can act for B.
 */
public class ConjunctivePrincipal extends Principal {
	Set<BasicPrincipal> set;
	
	public ConjunctivePrincipal(String s) {
		super(s);
		set = new HashSet<BasicPrincipal>();
		
		String[] comp = s.split("&");
		for (String c : comp) {
			set.add(new BasicPrincipal(c));
		}
	}
	
    public Set<BasicPrincipal> conjuncts() {
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
    	// cp actsfor granter if at least one of the conjucts act for granter
        for (BasicPrincipal bp : set) {
            if (bp.actsFor(p)) {
                return true;                    
            }
        }
        return false;
    }
}
