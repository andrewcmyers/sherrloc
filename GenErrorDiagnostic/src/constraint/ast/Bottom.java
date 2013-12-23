package constraint.ast;


public class Bottom extends Constructor {
	
	public Bottom(Position p) {
		super("⊥", 0, false, p);
	}
	
	@Override
	public Bottom getInstance () {
		return new Bottom(Position.EmptyPosition());
	}
		
	@Override
	public boolean isBottom() {
		return true;
	}
	
	@Override
	public Element getBaseElement() {
		return new Bottom(Position.EmptyPosition());
	}
}
