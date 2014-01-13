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
	
    /* a few print functions
     * an element in general has: element, snippet, location
     * toString prints code snippet + location
     * toSnippetString prints the code snippet corresponding to the element when available
     * toDetailString prints the element itself
     * toDotString prints Dot-friendly strings
     */
	abstract public String toString ();
	
	public String toSnippetString () {
		if (pos.isEmpty())
			return name;
		else
			return pos.snippet;
	}		
	
	public String toDetailString()  {
		return toSnippetString()+pos.toString();
	}
	
	public boolean isStart() {
		return true;
	}
	
	public boolean isEnd() {
		return true;
	}
	
	abstract public boolean trivialEnd ();

	abstract public String toDotString ();
	
	abstract public List<Variable> getVars ();
	
	abstract public boolean hasVars ();
	
	abstract public boolean equals (Object o);
			
	abstract public boolean isBottom();
	
	abstract public boolean isTop();
	
	// all duplicated elements should return the base element, where version number is 0 by this function
	abstract public Element getBaseElement();
}
