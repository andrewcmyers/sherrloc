package constraint.graph.visitor;

import constraint.graph.Node;

public interface NodeVisitor {
	public void discoverVertex(Node n);

	public void visit(Node n);

	public void leaveVertex(Node n);
}
