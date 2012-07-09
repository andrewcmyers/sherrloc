package constraint.ast;

public abstract class Principal {
	String name;

	public Principal(String s) {
		name = s;
	}
	
	public abstract boolean isTop();
	
	public abstract boolean isBottom();
	
	public abstract boolean actsFor (BasicPrincipal p);
	
	public String toString () {
		return name;
	}
}
