package constraint.ast;

public class BasicPrincipal extends Principal {
	
	public BasicPrincipal(String s) {
		super(s);
	}
	
	public boolean isBottom () {
		return name.equals("_");
	}
	
	public boolean isTop () {
		return name.equals("*");
	}
	
	public boolean actsFor (BasicPrincipal p) {
		return this.isTop() || p.isBottom() || this.name.equals(p.name);
	}
}
