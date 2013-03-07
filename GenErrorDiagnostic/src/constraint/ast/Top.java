package constraint.ast;


public class Top extends Constructor {

	public Top() {
		super("*", 0, false);
	}
	
	@Override
	public Constructor getInstance() {
		return new Top();
	}	
	
	@Override
	public boolean isTop() {
		return true;
	}
	
	@Override
	public Element getBaseElement() {
		Top ret = new Top();
		ret.id = 0;
		return ret;
	}
}
