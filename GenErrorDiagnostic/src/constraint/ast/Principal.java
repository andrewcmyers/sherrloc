package constraint.ast;

public abstract class Principal {
	String name;

	public Principal(String s) {
		name = s;
	}
	
	public abstract boolean isTop();
	
	public abstract boolean isBottom();
	
	public abstract boolean actsFor (BasicPrincipal p, Environment env);
	
	public String toString () {
		return name;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof Principal) {
			return name.equals(((Principal)obj).name);
		}
		return false;
	}
}
