package constraint.ast;


public class Bottom extends Constructor {
	static Bottom ins = null;
	
	private Bottom() {
		super("_", 0, false);
	}
	
	public static Bottom getBot () {
		if (ins==null) {
			ins = new Bottom();
		}
		return ins;
	}
	
	@Override
	public int hashCode() {
		return 5741;
	}
	
}
