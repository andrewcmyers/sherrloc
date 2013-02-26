package constraint.ast;

import java.util.List;

public abstract class Element {
	String name;
	Position pos;
    double succCount;
	
	public Element(String name, Position pos) {
		this.name = name;
		this.pos = pos;
        succCount = 0;
	}
	
	public String toString () {
		return toHTMLString()+pos.toString();
	}
	
	public String toHTMLString () {
		if (pos.isEmpty())
			return name;
		else
			return pos.snippet;
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
	
    public void incSuccCounter (int i) {
        succCount += i;
    }
    
    public double getSuccCounter () {
		return succCount;
	}
	
	abstract public String toDotString ();
	
	abstract public List<Variable> getVars ();
	
	abstract public boolean hasVars ();
	
	abstract public boolean equals (Object o);
	
	abstract public String toDetailString();
		
	abstract public boolean isStart();
	
	abstract public boolean isEnd();
	
	abstract public boolean isBottom();
	
	abstract public boolean isTop();
	
	// all duplicated elements should return the base element, where version number is 0 by this function
	abstract public Element getBaseElement();
}
