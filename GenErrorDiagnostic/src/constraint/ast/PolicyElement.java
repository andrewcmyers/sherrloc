package constraint.ast;

public class PolicyElement extends Constructor {
	String owner;
	String reader;
	
	public PolicyElement(String policy, String name, int arity) {
		super(name, arity);
		// for now, just parse a policy from string
		String[] result = policy.split("->");
		owner = result[0];
		if (result.length>1)
			reader = result[1];
		else
			reader = "_";
	}
	
	String owner() {
		return owner;
	}
	
    String reader() {
    	return reader;
    }
    
    boolean isBottom () {
    	return owner.equals("_")&&reader.equals("_");
    }
    
    boolean isTop () {
    	return owner.equals("*")&&reader.equals("*");
    }
    
    public static void main(String[] args) {
		PolicyElement ele = new PolicyElement("_->","_->",0);
		System.out.println(ele.owner);
		System.out.println(ele.reader);
	}
    
    public boolean leq_(Object o) {
    	if (this==o) return true;
    	
    	if (o instanceof PolicyElement) {
    		PolicyElement p = (PolicyElement) o;
	        if (this.isBottom() || p.isTop())
	            return true;
	
	        // if this policy is o:_, then o allows
	        // all principals to read info, and thus does
	        // not restrict who may read
	        if (reader.equals("_")) {
	            return true;
	        }
	            
	        // o' >= o
//	        if (!env.actsFor(p.owner(), this.owner)) {
//	            return false;
//	        }
//	        
//	        return env.actsFor(p.reader(), this.owner()) ||
//	               env.actsFor(p.reader(), this.reader());
    	}
    	return false;
    }
}
