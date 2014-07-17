package sherrloc.constraint.ast;

import java.util.List;

import sherrloc.graph.Variance;

/**
 * This class represents an application to a constructor, possibly with no
 * parameters (e.g., list int, int)
 * 
 * The constructor can either be concrete {@link Constructor}, or abstract
 * {@link Variable}
 */
public abstract class Application extends EnumerableElement {

	public Application(String name, List<Element> elements) {
		super(name, elements);
	}
	
	/**
	 * @return True if the parameters are contravariant
	 */
	public abstract Variance getVariance();
	
	/**
	 * Return a new application where e1 is replaced with e2
	 */
	public abstract Application replace (Element e1, Element e2);
}
