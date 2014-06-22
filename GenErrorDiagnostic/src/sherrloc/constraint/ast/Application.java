package sherrloc.constraint.ast;

import java.util.List;
import java.util.Map;

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
	public abstract boolean isContraVariant();
}
