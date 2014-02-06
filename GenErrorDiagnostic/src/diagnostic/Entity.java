package diagnostic;

import java.util.Map;

import constraint.graph.ElementNode;
import constraint.graph.Node;

/**
 * Entities are the basic units of error report. In OCaml, the entities are
 * program expressions; in Jif, the entities can be security labels or
 * information-flow constraints.
 * <p>
 * Method <code>matches</code> tests if an element node represents the entity 
 * <p>
 * Extend this class for other possibilities, such
 * as a combination of both.
 */
public abstract class Entity {
	
	private int succ;	// # satisfiable paths using the entity
	
	public Entity(int succ) {
		this.succ = succ;
	}
	
	/**
	 * @return # satisfiable paths using the entity
	 */
	public int getSuccCount() {
		return succ;
	}
	
	/**
	 * Returns true if the element node represents the entity
	 */
	public abstract boolean matches(ElementNode n);
	
	/**
	 * Pretty print the entity for HTML
	 */
	public abstract void toHTML (Map<String, Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf);
	
	/**
	 * Pretty print the entity for console
	 */
	public abstract void toConsole (Map<String, Node> exprMap, StringBuffer locBuf, StringBuffer exprBuf);
	
	/**
	 * Forces subclass to implement equals
	 */
	public abstract boolean equals(Object other);
	
	/**
	 * Forces subclass to implement hashCode
	 */
	public abstract int hashCode();
}
