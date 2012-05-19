package constraint.ast;

public enum Relation {
	LEQ, EQ;
	
	public String toString() {
		String s = super.toString();
		if (s.equals("LEQ")) 
			return "<";
		else 
			return "=";
	}
};
