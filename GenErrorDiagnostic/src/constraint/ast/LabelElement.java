package constraint.ast;

import java.util.List;

public class LabelElement extends Constructor {
	String owner;
	String reader;
	
	public LabelElement(String policy) {
		super(policy, 0);
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
		LabelElement ele = new LabelElement("_->");
		System.out.println(ele.owner);
		System.out.println(ele.reader);
	}
    
    @Override
    public boolean equals(Object o) {
    	if (o instanceof LabelElement) {
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
    	if (this==o) return true;
    	
    	if (o instanceof LabelElement) {
    		LabelElement p = (LabelElement) o;
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
    
    @Override
    public boolean isStart() {
    	return true;
    }
    
    @Override
    public boolean isEnd() {
    	return true;
    }
}
