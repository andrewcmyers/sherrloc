package constraint.ast;


public class Top extends Constructor {
	static Top ins=null;
	
	private Top() {
		super("*", 0, false);
	}
	
	public static Top getTop () {
		if (ins==null)
			ins =  new Top();
		return ins;
	}
	
	@Override
	public int hashCode() {
		return 2547;
	}
	
	@Override
	public boolean isTop() {
		return true;
	}
}
