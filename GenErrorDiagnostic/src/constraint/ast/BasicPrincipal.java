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
	
	public boolean actsFor (BasicPrincipal p, Environment env) {
		return this.isTop() || p.isBottom() || env.actsFor(this.name, p.name);
	}
}
