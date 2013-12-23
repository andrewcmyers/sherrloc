package constraint.ast;


public class Top extends Constructor {

	public Top(Position p) {
		super("⊤", 0, false, p);
	}
	
	@Override
	public Constructor getInstance() {
		return new Top(Position.EmptyPosition());
	}	
	
	@Override
	public boolean isTop() {
		return true;
	}
	
	@Override
	public Element getBaseElement() {
		return new Top(Position.EmptyPosition());
	}
}
