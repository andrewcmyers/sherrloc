package constraint.ast;


public class Top extends Constructor {

	public Top() {
		super("*", 0, false);
	}
	
	@Override
	public int hashCode() {
		return 2547;
	}
}
