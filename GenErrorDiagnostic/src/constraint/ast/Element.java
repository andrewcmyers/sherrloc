package constraint.ast;

import java.util.List;

public abstract class Element {
	String name;
	Position pos;
	
	public Element(String name, Position pos) {
		this.name = name;
		this.pos = pos;
	}
	
	public String toString () {
		return name+pos.toString();
	}
	
	public String getName () {
		return name;
	}
		
	public Position getPosition () {
		return pos;
	}
	
	public void setPosition (Position pos) {
		this.pos = pos;
	}
	
	abstract public String toDotString ();
	
	abstract public List<Variable> getVars ();
	
	abstract public boolean hasVars ();
	
	abstract public boolean equals (Object o);
		
	abstract public boolean isStart();
	
	abstract public boolean isEnd();
	
}
