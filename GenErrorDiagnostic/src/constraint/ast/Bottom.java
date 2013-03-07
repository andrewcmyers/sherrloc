package constraint.ast;


public class Bottom extends Constructor {
	
	public Bottom() {
		super("_", 0, false);
	}
	
	public Bottom getInstance () {
		return new Bottom();
	}
		
	@Override
	public boolean isBottom() {
		return true;
	}
	
	@Override
	public Element getBaseElement() {
		Bottom ret = new Bottom();
		ret.id = 0;
		return ret;
	}
}
