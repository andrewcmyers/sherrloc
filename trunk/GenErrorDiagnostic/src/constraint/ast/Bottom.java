package constraint.ast;


public class Bottom extends Constructor {

	public Bottom() {
		super("_", 0, false);
	}
	
	@Override
	public int hashCode() {
		return 5741;
	}
	
}