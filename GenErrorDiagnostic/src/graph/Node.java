package graph;

import constraint.ast.Element;

/**
 * A node in the constraint graph represents an element in the constraint
 */
public class Node {
    private int index; 				// index in graph, used to quickly retrieve a node from graph
    private Element element;
    public boolean shouldprint;
    boolean iscause;
          
	/**
	 * @param index
	 *            Index of all nodes in graph. Used to quickly retrieve a node
	 * @param element
	 *            An element that the constructed node represents
	 */
    public Node(int index, Element element) {
        shouldprint = false;
        iscause = false;
        this.index = index;
        this.element = element;
    }
        
    void setCause () {
        iscause = true;
    }
    
    public boolean isCause () {
        return iscause;
    }
            
    public int getIndex() {
		return index;
	}
    
    public String getName () {
    	return element.toDetailString();
    }
    
    public Element getElement() {
		return element;
	}
            
    public String toString() {
        return getName();
    }
    
    public void incSuccCounter () {
        element.incSuccCounter(1);
    }
    
    public void incNestedCounter (int i) {
        element.incSuccCounter(i);
    }
    
    public int getSuccCounter () {
		return element.getSuccCounter();
	}
    
    public String getUid () {
    	return "v"+index;
    }
    
    public String printNodeToDotString () {
        if (isCause())
            return  getUid() + " [style=filled, fillcolor=yellow, label=\"" + element.toDotString()+ "\"];\n";
        else
            return  getUid() + " [label=\"" + element.toDotString()+ "\"];\n";
    }
}
