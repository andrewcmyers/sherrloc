package constraint.ast;

public class ConstructorFactory {
	
	public static Constructor getConstructor (String name, int arity) {
//		return new Constructor(name, arity);
		return new PolicyElement(name, name, arity);
	}
}
