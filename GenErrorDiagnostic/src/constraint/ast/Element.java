package constraint.ast;

import java.util.List;

/**
 * Constraint element.
 */
public abstract class Element {
	protected String name;
	protected Position pos;
	private int succCount; 		// # satisfiable path using this element
	
	/**
	 * @param name Element name
	 * @param pos Element position in the source file
	 */
	public Element(String name, Position pos) {
		this.name = name;
		this.pos = pos;
        succCount = 0;
	}
	
	/**
	 * @return Element name
	 */
	public String getName () {
		return name;
	}
		
	/**
	 * @return Element position in the source file
	 */
	public Position getPosition () {
		return pos;
	}
	
	/**
	 * @param pos Element position in the source file
	 */
	public void setPosition (Position pos) {
		this.pos = pos;
	}
	
    public void incSuccCounter (int i) {
        succCount += i;
    }
    
    /**
     * @return # satisfiable path using this element
     */
    public int getSuccCounter () {
		return succCount;
	}
	
    /** a few print functions */

    abstract public String toString ();
	
	/**
	 * @return The code snippet corresponding to the element when available.
	 *         Return element name otherwise.
	 */
	public String toSnippetString () {
		if (pos.isEmpty())
			return name;
		else
			return pos.snippet;
	}		
	
	/**
	 * @return The code snippet of the element, and its position
	 */
	public String toDetailString()  {
		return toSnippetString()+pos.toString();
	}
	
	/**
	 * @return Dot-friendly string of the element
	 */
	abstract public String toDotString ();

	/**
	 * @return True when the satisfiability of a path is trivial when it starts
	 *         with the element (e.g., a variable and a join element)
	 */
	abstract public boolean trivialStart ();
	
	/**
	 * @return True when the satisfiability of a path is trivial when it ends
	 *         with the element (e.g., a variable and a meet element)
	 */
	abstract public boolean trivialEnd ();
	
	/**
	 * @return All constraint variables used in the element
	 */
	abstract public List<Variable> getVars ();
	
	abstract public boolean hasVars ();
	
	abstract public boolean equals (Object o);
			
	abstract public boolean isBottom();
	
	abstract public boolean isTop();
	
	// all duplicated elements should return the base element, where version number is 0 by this function
	abstract public Element getBaseElement();
}
