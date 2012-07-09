package constraint.ast;

import java.util.List;

public class LabelElement extends Constructor {
	BasicPrincipal owner;
	Principal reader;
	boolean isPolicy;
	
	public LabelElement(String policy) {
		super(policy, 0);
		// for now, just parse a policy from string
		if (policy.contains("->")) {
			String[] result = policy.split("->");
			owner = new BasicPrincipal(result[0]);
			String s;
			if (result.length>1)
				s = result[1];
			else
				s = "*";
			if (s.contains(",")) {
				reader = new DisjunctivePrincipal(s);
			}
			else if (s.contains("&")) {
				reader = new ConjunctivePrincipal(s);
			}
			else
				reader = new BasicPrincipal(s);
			isPolicy = true;
		}
		else {
			isPolicy = false;
		}
	}
    
    public boolean isComparable () {
		return isPolicy;
	}
    
    boolean isBottom () {
    	return owner.isBottom()&&reader.isBottom();
    }
    
    boolean isTop () {
    	return owner.isTop()&&reader.isTop();
    }
    
    public static void main(String[] args) {
		LabelElement ele = new LabelElement("_->");
		System.out.println(ele.owner);
		System.out.println(ele.reader);
	}
    
    public String toString () {
    	if (isComparable()) {
    		return owner + "->" + reader;
    	}
    	else
    		return name;
    }
    
    @Override
    public boolean equals(Object o) {
    	if (this==o)
    		return true;
    	
    	if (o instanceof LabelElement && this.isComparable() && ((LabelElement)o).isComparable()) {
    		return owner.equals(((LabelElement)o).owner)&&reader.equals(((LabelElement)o).reader);
    	}
    	return false;
    }
    
    @Override
    public List<Variable> getVars() {
    	return null;
    }
    
    @Override
    public boolean hasVars() {
    	return false;
    }
    
    public boolean leq_(Object o) {
    	if (this==o) 
    		return true;
    	
    	if (!isComparable())
    		return false;
    	
    	if (o instanceof LabelElement && ((LabelElement)o).isComparable()) {
    		LabelElement p = (LabelElement) o;
	        if (this.isBottom() || p.isTop())
	            return true;

	        // if this policy is o:_, then o allows
	        // all principals to read info, and thus does
	        // not restrict who may read
	        if (reader.isBottom()) {
	            return true;
	        }
	            
	        // o' >= o
	        if (!p.owner.actsFor(owner)) {
	        	return false;
	        }
	        
	        if (p.reader.actsFor(owner))
	        	return true;
	        
	        if (reader instanceof ConjunctivePrincipal) {
	        	ConjunctivePrincipal cp = (ConjunctivePrincipal)reader;
	            // actor actsfor cp if actor actsfor all conjuncts
	            for (BasicPrincipal bp : cp.conjuncts()) {
	                if (!p.reader.actsFor(bp)) {
	                    return false; 
	                }                
	            }
	            return true;
	        }
	        else if (reader instanceof DisjunctivePrincipal) {
	        	DisjunctivePrincipal dp = (DisjunctivePrincipal)reader;
	            // actor actsfor dp if there is one disjunct that actor can act for
	            for (BasicPrincipal bp : dp.disjuncts()) {
	                if (p.reader.actsFor(bp)) {
	                    return true;                    
	                }
	            }
	            return false;
	        }
	        else {
	        	return p.reader.actsFor((BasicPrincipal)reader);
	        }
//	        if (!env.actsFor(p.owner(), this.owner)) {
//	            return false;
//	        }
//	        
//	        return env.actsFor(p.reader(), this.owner()) ||
//	               env.actsFor(p.reader(), this.reader());
    	}
    	return false;
    }
    
    @Override
    public boolean isStart() {
    	return true;
    }
    
    @Override
    public boolean isEnd() {
    	return true;
    }
}
